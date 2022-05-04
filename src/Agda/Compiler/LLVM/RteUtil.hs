module Agda.Compiler.LLVM.RteUtil where

import Agda.Compiler.LLVM.Syntax

typeThunk :: LLVMType
typeThunk = LLVMTRef $ LLVMIdent "agda.struct.thunk"

typeThunkPtr :: LLVMType
typeThunkPtr = LLVMPtr typeThunk

typeThunkEval :: LLVMType
typeThunkEval = LLVMTRef $ LLVMIdent "agda.struct.thunk.eval"

typeThunkEvalPtr :: LLVMType
typeThunkEvalPtr = LLVMPtr typeThunkEval

refAllocThunk :: LLVMRef
refAllocThunk =
  LLVMGlobal {refName = LLVMIdent "agda.alloc.thunk", refType = LLVMFn {fnRet = typeThunkPtr, fnParams = []}}

typeVoidPtr :: LLVMType
typeVoidPtr = LLVMPtr $ LLVMSizedInt 8
