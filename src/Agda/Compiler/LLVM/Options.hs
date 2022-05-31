module Agda.Compiler.LLVM.Options where

import Agda.Compiler.Backend (EvaluationStrategy(..), Flag)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

deriving instance Generic EvaluationStrategy

deriving instance NFData EvaluationStrategy

data LLVMOptions =
  LLVMOptions
    { llvmEvaluationStrategy :: EvaluationStrategy
    , llvmClangDebug :: Bool
    }
  deriving (Generic, NFData)

data LLVMEvaluationStrategy
  = LLVMEager
  | LLVMLazy

defaultLLVMOptions :: LLVMOptions
defaultLLVMOptions = LLVMOptions {llvmEvaluationStrategy = LazyEvaluation, llvmClangDebug = True}

optionFlagEvaluationStrategy :: EvaluationStrategy -> Flag LLVMOptions
optionFlagEvaluationStrategy s o = return $ o {llvmEvaluationStrategy = s}

optionFlagClangDebug :: Bool -> Flag LLVMOptions
optionFlagClangDebug s o = return $ o {llvmClangDebug = s}
