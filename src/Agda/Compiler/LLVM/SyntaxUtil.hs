module Agda.Compiler.LLVM.SyntaxUtil where

import Agda.Compiler.LLVM.Syntax (LLVMIdent(LLVMIdent), LLVMInstruction)

-- | Safely create an identifier
llvmIdent :: String -> LLVMIdent
-- TODO: account for illegal chars
llvmIdent = LLVMIdent

llvmRecord :: String -> LLVMInstruction -> (Maybe LLVMIdent, LLVMInstruction)
llvmRecord ident = (Just $ LLVMIdent ident, )

llvmDiscard :: LLVMInstruction -> (Maybe LLVMIdent, LLVMInstruction)
llvmDiscard = (Nothing, )
