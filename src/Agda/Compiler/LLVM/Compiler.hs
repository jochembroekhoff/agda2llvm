{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse_" #-}
module Agda.Compiler.LLVM.Compiler where

import Agda.Compiler.Backend
import Agda.Compiler.Common (compileDir)
import Agda.Compiler.LLVM.ASyntax
import Agda.Compiler.LLVM.ASyntaxUtil (aIdentFromQName)
import Agda.Compiler.LLVM.AbstractOpt (AbstractOptimize(abstractOptimize))
import Agda.Compiler.LLVM.AbstractToLLVM (AToLlvm(aToLlvm))
import Agda.Compiler.LLVM.Options
  ( LLVMOptions(llvmEvaluationStrategy, llvmVerboseRuntime)
  , defaultLLVMOptions
  , optionFlagClangDebug
  , optionFlagEvaluationStrategy
  , optionFlagVerboseRuntime
  )
import Agda.Compiler.LLVM.Pprint (LLVMPretty(llvmPretty))
import Agda.Compiler.LLVM.RteUtil
import Agda.Compiler.LLVM.Syntax
import Agda.Compiler.LLVM.SyntaxUtil (llvmDiscard, llvmIdent)
import Agda.Compiler.LLVM.Tables (computeCtorIdent)
import Agda.Compiler.LLVM.ToAbstractIntermediate (ToAbstractIntermediate(toA))
import Agda.Compiler.LLVM.Wiring
  ( callLLVM
  , fileIntermediateAux
  , fileIntermediateMod
  , writeIntermediateAux
  , writeIntermediateModule
  )
import Agda.Interaction.Options (ArgDescr(NoArg), OptDescr(Option))
import Agda.TypeChecking.Primitive (getBuiltinName)
import Agda.Utils.Pretty (prettyShow)
import Agda.Utils.Tuple (mapFstM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Set as Set
import Debug.Trace (trace)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

llvmBackendName = "LLVM"

llvmBackend :: Backend
llvmBackend = Backend llvmBackend'

llvmBackend' :: Backend' LLVMOptions LLVMOptions LLVMEnv LLVMModule [AEntry]
llvmBackend' =
  Backend'
    { backendName = llvmBackendName
    , backendVersion = Nothing
    , options = defaultLLVMOptions
    , commandLineFlags = llvmCommandLineFlags
    , isEnabled = const True
    , preCompile = return
    , postCompile = llvmPostCompile
    , preModule = \_ _ _ _ -> return $ Recompile LLVMEnv {}
    , postModule = llvmPostModule
    , compileDef = llvmCompileDef
    , scopeCheckingSuffices = False
    , mayEraseType = const $ return True
    }

--- CLI flags ---
llvmCommandLineFlags :: [OptDescr (Flag LLVMOptions)]
llvmCommandLineFlags =
  [ Option
      []
      ["lazy-evaluation"]
      (NoArg $ optionFlagEvaluationStrategy LazyEvaluation)
      "Use thunks at runtime to support lazy operations (default)"
  , Option
      []
      ["strict-evaluation"]
      (NoArg $ optionFlagEvaluationStrategy EagerEvaluation)
      "Evaluate all arguments eagerly"
  , Option [] ["clang-debug"] (NoArg $ optionFlagClangDebug True) "Call Clang with the debug configuration (default)"
  , Option [] ["clang-release"] (NoArg $ optionFlagClangDebug False) "Call Clang with the release profile, includes LTO"
  , Option [] ["verbose-runtime"] (NoArg $ optionFlagVerboseRuntime True) "Use verbose LLVM runtime (off by default)"
  ]

--- Envs ---
data LLVMEnv =
  LLVMEnv
    {
    }

--- Compilation start & finish handlers ---
llvmPostCompile :: LLVMOptions -> IsMain -> Map.Map ModuleName LLVMModule -> TCM ()
llvmPostCompile opts isMain modules = do
  modules' <- traverse (uncurry writeIntermediateModule) $ Map.toList modules
  auxMetaModule <- llvmAuxMetaModule
  auxMeta <- writeIntermediateAux "meta" auxMetaModule
  auxBuiltinRefsModule <- llvmAuxBuiltinRefsModule opts
  auxBuiltinRefs <- writeIntermediateAux "builtin_refs" auxBuiltinRefsModule
  callLLVM opts isMain (auxMeta : auxBuiltinRefs : modules')

--- Module & defs compilation ---
llvmPostModule :: LLVMOptions -> LLVMEnv -> IsMain -> ModuleName -> [[AEntry]] -> TCM LLVMModule
llvmPostModule opts _ main m defs = do
  d <- compileDir
  let m' = mnameToList m
  interm <- fileIntermediateMod m
  liftIO
    do putStrLn $ "Module: " ++ prettyShow m'
       putStrLn $ "IsMain: " ++ show (main == IsMain)
       putStrLn $ "CompileDir: " ++ show d
       putStrLn $ "IntermediateFile: " ++ show interm
  -- TODO: check that there is a definition @main iff main==IsMain
  -- TODO: add header entries (instead of string concat)
  let defs' = concatMap (llvmFromAbstract opts) $ concat defs
      defsImported = llvmThunkImports defs'
  return $ LLVMModule {entries = defsImported ++ defs'}

llvmCompileDef :: LLVMOptions -> LLVMEnv -> IsMain -> Definition -> TCM [AEntry]
llvmCompileDef _ _ isMain def = do
  res <- evalStateT (toA def) 0
  case res of
    Nothing -> return []
    Just (name, entries) -> return (entries ++ [AEntryMain name | thisDefIsTheMainOne isMain def])

thisDefIsTheMainOne :: IsMain -> Definition -> Bool
thisDefIsTheMainOne NotMain _ = False
thisDefIsTheMainOne IsMain def = prettyShow (qnameName $ defName def) == "main"

-- | Create LLVM entries for references that need an import
llvmThunkImports :: [LLVMEntry] -> [LLVMEntry]
llvmThunkImports entries = map declare (Set.toList implicitDeclarations)
  where
    definitions = Set.fromList $ mapMaybe findDefinition entries
    declarations = Set.fromList $ concatMap findDecls entries
    implicitDeclarations = declarations Set.\\ definitions
    findDefinition :: LLVMEntry -> Maybe LLVMIdent
    findDefinition LLVMFnDefn {fnSign = sign@LLVMFnSign {fnName = ident}}
      | sign == mkCreatorFnSign ident = Just ident
    findDefinition _ = Nothing
    findDecls :: LLVMEntry -> [LLVMIdent]
    findDecls LLVMFnDefn {body = body} = mapMaybe findDeclsInInstruction instructions
      where
        instructions = concatMap (\(LLVMBlock {blockInstructions = instrs}) -> map snd instrs) body
        findDeclsInInstruction :: LLVMInstruction -> Maybe LLVMIdent
        findDeclsInInstruction LLVMCall {callRef = LLVMGlobal {refName = ident, refType = refType}}
          | llvmIsImportable ident && refType == typeFnCreator = Just ident
        findDeclsInInstruction _ = Nothing
    findDecls _ = []
    declare :: LLVMIdent -> LLVMEntry
    declare ident =
      LLVMFnDecl [] $
      LLVMFnSign
        {fnName = ident, fnType = typeThunkPtr, fnArgs = [(typeFramePtr, llvmIdent "record")], fnArgsVariadic = False}

-- | Check if a given identifier is prefixed with @agda2llvm.@ (or the quoted variant).
--   This indicates that the symbol is generated by agda2llvm dynamically,
--   and can therefore be imported.
--   In contrast, identifiers such as @agda.prim.*@ cannot be imported, because they are
--   already brought into scope by default, or if applicable.
llvmIsImportable :: LLVMIdent -> Bool
llvmIsImportable (LLVMIdent identRaw) = any (`isPrefixOf` identRaw) ["agda2llvm.", "\"agda2llvm."]

llvmAuxMetaModule :: TCM LLVMModule
llvmAuxMetaModule = return $ LLVMModule []

llvmAuxBuiltinRefsModule :: LLVMOptions -> TCM LLVMModule
llvmAuxBuiltinRefsModule opts = do
  true <- getBuiltinName builtinTrue
  false <- getBuiltinName builtinFalse
  let true' = mkBuiltin "true" true
  let false' = mkBuiltin "false" false
  return $ LLVMModule (true' ++ false')
  where
    mkBuiltin :: String -> Maybe QName -> [LLVMEntry]
    mkBuiltin n Nothing =
      llvmFromAbstract opts $
      AEntryDirect
        { entryIdent = AIdentRaw $ "agda.builtin_refs.make_" ++ n
        , entryPushArg = False
        , entryBody = AError "builtin not bound, but still used"
        }
    mkBuiltin n (Just qn) =
      llvmFromAbstract opts $
      AEntryDirect
        { entryIdent = AIdentRaw $ "agda.builtin_refs.make_" ++ n
        , entryPushArg = False
        , entryBody = AMkValue $ AValueData {dataIdx = idx, dataCase = kase, dataArity = 0}
        }
      where
        (idx, kase) = computeCtorIdent $ aIdentFromQName qn

llvmFromAbstract :: LLVMOptions -> AEntry -> [LLVMEntry]
llvmFromAbstract opts = aToLlvm . (llvmEvaluationStrategy opts, ) . abstractOptimize
