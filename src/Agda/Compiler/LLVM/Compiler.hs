module Agda.Compiler.LLVM.Compiler where

import Agda.Compiler.Backend
import Agda.Compiler.Common (compileDir)
import Agda.Compiler.LLVM.Pprint (LLVMPretty(llvmPretty))
import Agda.Compiler.LLVM.Syntax
import Agda.Compiler.LLVM.SyntaxUtil (llvmIdent)
import Agda.Interaction.Options (OptDescr)
import Agda.Utils.Pretty (prettyShow)
import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import GHC.Generics (Generic)

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
    , postCompile = \_ _ _ -> return ()
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

--- Module & defs compilation ---
llvmPostModule :: LLVMOptions -> LLVMEnv -> IsMain -> ModuleName -> [Maybe LLVMEntry] -> TCM LLVMModule
llvmPostModule _ _ main m defs = do
  d <- compileDir
  let m' = mnameToList m
  liftIO $ do
    putStrLn $ "Module: " ++ prettyShow m'
    putStrLn $ "IsMain: " ++ show (main == IsMain)
    putStrLn $ "CompileDir: " ++ show d
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
