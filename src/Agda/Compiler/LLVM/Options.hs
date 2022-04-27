module Agda.Compiler.LLVM.Options where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data LLVMOptions =
  LLVMOptions
    {
    }
  deriving (Generic, NFData)

defaultLLVMOptions :: LLVMOptions
defaultLLVMOptions = LLVMOptions {}
