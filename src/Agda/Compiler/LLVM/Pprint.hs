module Agda.Compiler.LLVM.Pprint where

import Agda.Compiler.LLVM.Syntax
import Data.List

class LLVMPretty a where
  llvmPretty :: a -> String

instance LLVMPretty LLVMModule where
  llvmPretty (LLVMModule entries) = intercalate "," (map llvmPretty entries)

instance LLVMPretty LLVMEntry where
  llvmPretty (LLVMFnDecl fnSign) = "ahah"
  llvmPretty (LLVMFnDefn sdf sdfs) = "hehehe"
