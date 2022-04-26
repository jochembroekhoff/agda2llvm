module Agda.Compiler.LLVM.Compiler where

import Agda.Compiler.Backend
import Agda.Compiler.LLVM.Syntax
import Agda.Interaction.Options (OptDescr)
import Control.DeepSeq (NFData)
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
llvmPostModule _ _ main m defs = trace ("Defs: " ++ show defs) $ return $ LLVMModule {entries = []}

llvmCompileDef :: LLVMOptions -> LLVMEnv -> IsMain -> Definition -> TCM (Maybe LLVMEntry)
llvmCompileDef _ _ _ def = return $ Just LLVMFnDecl {fnSign = LLVMFnSign {fnName = "Hello", fnType = LLVMSizedInt 64}}
