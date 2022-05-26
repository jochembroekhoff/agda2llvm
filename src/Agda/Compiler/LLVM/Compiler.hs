{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse_" #-}
module Agda.Compiler.LLVM.Compiler where

import Agda.Compiler.Backend
import Agda.Compiler.Common (compileDir)
import Agda.Compiler.LLVM.ASyntax
import Agda.Compiler.LLVM.ASyntaxUtil (aIdentFromQName)
import Agda.Compiler.LLVM.AbstractToLLVM (AToLlvm(aToLlvm))
import Agda.Compiler.LLVM.Options (LLVMOptions, defaultLLVMOptions)
import Agda.Compiler.LLVM.Pprint (LLVMPretty(llvmPretty))
import Agda.Compiler.LLVM.RteUtil
import Agda.Compiler.LLVM.Syntax
  ( LLVMBlock(LLVMBlock, blockInstructions)
  , LLVMEntry(..)
  , LLVMFnSign(LLVMFnSign, fnArgs, fnName, fnType)
  , LLVMIdent(..)
  , LLVMInstruction(LLVMCall, callRef)
  , LLVMModule(..)
  , LLVMRef(LLVMGlobal, refName, refType)
  )
import Agda.Compiler.LLVM.SyntaxUtil (llvmIdent)
import Agda.Compiler.LLVM.Tables (computeCtorIdent)
import Agda.Compiler.LLVM.ToAbstractIntermediate (ToAbstractIntermediate(toA))
import Agda.Compiler.LLVM.Wiring
  ( callLLVM
  , fileIntermediateAux
  , fileIntermediateMod
  , writeIntermediateAux
  , writeIntermediateModule
  )
import Agda.Interaction.Options (OptDescr)
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
    , preCompile = llvmPreCompile
    , postCompile = llvmPostCompile
    , preModule = \_ _ _ _ -> return $ Recompile LLVMEnv {}
    , postModule = llvmPostModule
    , compileDef = llvmCompileDef
    , scopeCheckingSuffices = False
    , mayEraseType = const $ return True
    }

--- CLI flags ---
llvmCommandLineFlags :: [OptDescr (Flag LLVMOptions)]
llvmCommandLineFlags = []

--- Envs ---
data LLVMEnv =
  LLVMEnv
    {
    }

--- Compilation start & finish handlers ---
llvmPreCompile :: LLVMOptions -> TCM LLVMOptions
llvmPreCompile = return

llvmPostCompile :: LLVMOptions -> IsMain -> Map.Map ModuleName LLVMModule -> TCM ()
llvmPostCompile opts isMain modules = do
  modules' <- traverse (uncurry writeIntermediateModule) $ Map.toList modules
  auxMetaModule <- llvmAuxMetaModule
  auxMeta <- writeIntermediateAux "meta" auxMetaModule
  auxBuiltinRefsModule <- llvmAuxBuiltinRefsModule
  auxBuiltinRefs <- writeIntermediateAux "builtin_refs" auxBuiltinRefsModule
  callLLVM opts isMain (auxMeta : auxBuiltinRefs : modules')

--- Module & defs compilation ---
llvmPostModule :: LLVMOptions -> LLVMEnv -> IsMain -> ModuleName -> [[AEntry]] -> TCM LLVMModule
llvmPostModule _ _ main m defs = do
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
  let defs' = concatMap aToLlvm $ concat defs
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
      LLVMFnDecl $ LLVMFnSign {fnName = ident, fnType = typeThunkPtr, fnArgs = [(typeFramePtr, llvmIdent "record")]}

-- | Check if a given identifier is prefixed with @agda2llvm.@ (or the quoted variant).
--   This indicates that the symbol is generated by agda2llvm dynamically,
--   and can therefore be imported.
--   In contrast, identifiers such as @agda.prim.*@ cannot be imported, because they are
--   already brought into scope by default, or if applicable.
llvmIsImportable :: LLVMIdent -> Bool
llvmIsImportable (LLVMIdent identRaw) = any (`isPrefixOf` identRaw) ["agda2llvm.", "\"agda2llvm."]

llvmAuxMetaModule :: TCM LLVMModule
llvmAuxMetaModule = return $ LLVMModule []

llvmAuxBuiltinRefsModule :: TCM LLVMModule
llvmAuxBuiltinRefsModule = do
  Just true <- getBuiltinName builtinTrue
  Just false <- getBuiltinName builtinFalse
  let (idxTrue, kaseTrue) = computeCtorIdent (aIdentFromQName true)
  let (idxFalse, kaseFalse) = computeCtorIdent (aIdentFromQName false)
  let makeTrue =
        aToLlvm $
        AEntryDirect
          { entryIdent = AIdentRaw "agda.builtin_refs.make_true"
          , entryPushArg = False
          , entryBody = AMkValue $ AValueData {dataIdx = idxTrue, dataCase = kaseTrue, dataArity = 0}
          }
  let makeFalse =
        aToLlvm $
        AEntryDirect
          { entryIdent = AIdentRaw "agda.builtin_refs.make_false"
          , entryPushArg = False
          , entryBody = AMkValue $ AValueData {dataIdx = idxFalse, dataCase = kaseFalse, dataArity = 0}
          }
  return $ LLVMModule (makeTrue ++ makeFalse)
