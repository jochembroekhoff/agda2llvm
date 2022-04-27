{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse_" #-}
module Agda.Compiler.LLVM.Compiler where

import Agda.Compiler.Backend
import Agda.Compiler.Common (compileDir)
import Agda.Compiler.LLVM.Pprint (LLVMPretty(llvmPretty))
import Agda.Compiler.LLVM.Syntax
import Agda.Compiler.LLVM.SyntaxUtil (llvmIdent)
import Agda.Compiler.LLVM.Wiring (fileIntermediate)
import Agda.Interaction.Options (OptDescr)
import Agda.Utils.Pretty (prettyShow)
import Agda.Utils.Tuple (mapFstM)
import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import GHC.Generics (Generic)
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

--- Options ---
data LLVMOptions =
  LLVMOptions
    {
    }
  deriving (Generic, NFData)

defaultLLVMOptions :: LLVMOptions
defaultLLVMOptions = LLVMOptions {}

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
  liftIO $ do
    traverse (createDirectoryIfMissing True . takeDirectory . fst) modules''
    traverse (uncurry writeFile) modules''
  liftIO $ do putStrLn "TODO: call llvm/clang, respect @isMain@"

--- Module & defs compilation ---
llvmPostModule :: LLVMOptions -> LLVMEnv -> IsMain -> ModuleName -> [Maybe LLVMEntry] -> TCM LLVMModule
llvmPostModule _ _ main m defs = do
  d <- compileDir
  let m' = mnameToList m
  interm <- fileIntermediate m
  liftIO $ do
    putStrLn $ "Module: " ++ prettyShow m'
    putStrLn $ "IsMain: " ++ show (main == IsMain)
    putStrLn $ "CompileDir: " ++ show d
    putStrLn $ "IntermediateFile: " ++ show interm
  return $ LLVMModule {entries = catMaybes defs}

llvmCompileDef :: LLVMOptions -> LLVMEnv -> IsMain -> Definition -> TCM (Maybe LLVMEntry)
llvmCompileDef _ _ _ def = do
  let t = LLVMPtr $ LLVMArray 4 (LLVMStruct True [LLVMSizedInt 8])
  let res =
        LLVMFnDefn
          { fnSign = LLVMFnSign {fnName = llvmIdent "Hello", fnType = t, fnArgs = []}
          , body = [LLVMBlock "begin" [LLVMRet $ Just $ LLVMLit $ LLVMNull t]]
          }
  return (Just res)
