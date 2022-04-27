module Agda.Compiler.LLVM.SyntaxUtil where

import Agda.Compiler.LLVM.Syntax (LLVMIdent(LLVMIdent))

-- | Safely create an identifier
llvmIdent :: String -> LLVMIdent
-- TODO: account for illegal chars
llvmIdent = LLVMIdent
