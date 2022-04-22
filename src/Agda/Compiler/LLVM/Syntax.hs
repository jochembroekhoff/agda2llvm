module Agda.Compiler.LLVM.Syntax where

data LLVMModule =
  LLVMModule
    { modDefs :: [LLVMDef]
    }

data LLVMDef =
  LLVMDef
    { name :: String
    }
