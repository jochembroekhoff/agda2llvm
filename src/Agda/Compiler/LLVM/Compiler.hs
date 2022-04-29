{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse_" #-}
module Agda.Compiler.LLVM.Compiler where

import Agda.Compiler.Backend
import Agda.Compiler.Common (compileDir)
import Agda.Compiler.LLVM.Options (LLVMOptions, defaultLLVMOptions)
import Agda.Compiler.LLVM.Pprint (LLVMPretty(llvmPretty))
import Agda.Compiler.LLVM.Syntax
import Agda.Compiler.LLVM.SyntaxUtil (llvmIdent)
import Agda.Compiler.LLVM.Wiring (callLLVM, fileIntermediate)
import Agda.Interaction.Options (OptDescr)
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Lens
import Agda.Utils.Pretty (prettyShow)
import Agda.Utils.Tuple (mapFstM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

llvmBackendName = "LLVM"

llvmBackend :: Backend
llvmBackend = Backend llvmBackend'

llvmBackend' :: Backend' LLVMOptions LLVMOptions LLVMEnv LLVMModule (Maybe LLVMEntry)
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

llvmPostCompile :: LLVMOptions -> IsMain -> Map ModuleName LLVMModule -> TCM ()
llvmPostCompile opts isMain modules = do
  let modules' = Map.map llvmPretty modules
  modules'' <- traverse (mapFstM fileIntermediate) $ Map.toList modules'
  liftIO
    do traverse (createDirectoryIfMissing True . takeDirectory . fst) modules''
       traverse (uncurry writeFile) modules''
  callLLVM opts isMain (map fst modules'')

--- Module & defs compilation ---
llvmPostModule :: LLVMOptions -> LLVMEnv -> IsMain -> ModuleName -> [Maybe LLVMEntry] -> TCM LLVMModule
llvmPostModule _ _ main m defs = do
  d <- compileDir
  let m' = mnameToList m
  interm <- fileIntermediate m
  liftIO
    do putStrLn $ "Module: " ++ prettyShow m'
       putStrLn $ "IsMain: " ++ show (main == IsMain)
       putStrLn $ "CompileDir: " ++ show d
       putStrLn $ "IntermediateFile: " ++ show interm
  return $ LLVMModule {entries = catMaybes defs}

llvmCompileDef :: LLVMOptions -> LLVMEnv -> IsMain -> Definition -> TCM (Maybe LLVMEntry)
llvmCompileDef _ _ _ def = do
  toLlvm def

class ToLlvm a b where
  toLlvm :: a -> TCM b

instance ToLlvm Definition (Maybe LLVMEntry) where
  toLlvm def = do
    let qn = defName def
        qn' = prettyShow $ qnameName qn
    case theDef def of
      Axiom {} -> return Nothing
      GeneralizableVar {} -> return Nothing
      d@Function {}
        | d ^. funInline -> return Nothing
      Function {} -> do
        let t = LLVMPtr $ LLVMArray 4 (LLVMStruct True [LLVMSizedInt 8])
        tl <- toTreeless LazyEvaluation qn
        liftIO
          do putStr "FUNCTION: "
             putStrLn $ prettyShow qn
             putStrLn $ prettyShow tl
        return $
          Just $
          LLVMFnDefn
            { fnSign = LLVMFnSign {fnName = llvmIdent qn', fnType = t, fnArgs = []}
            , body = [LLVMBlock "begin" [LLVMRet $ Just $ LLVMLit $ LLVMNull t]]
            }
      Primitive {} -> return Nothing
      PrimitiveSort {} -> return Nothing
      Datatype {} -> return Nothing
      Record {} -> return Nothing
      Constructor {conSrcCon = chead, conArity = nargs} -> do
        liftIO
          do putStr "CONSTRUCTOR: "
             putStrLn $ prettyShow qn
             putStrLn $ prettyShow chead
             print nargs
        return Nothing
      AbstractDefn {} -> __IMPOSSIBLE__
      DataOrRecSig {} -> __IMPOSSIBLE__
