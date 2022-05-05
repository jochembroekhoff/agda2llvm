module Agda.Compiler.LLVM.RteUtil where

import Agda.Compiler.LLVM.Syntax

--- Data Base Struct ---
typeDataBase :: LLVMType
typeDataBase = LLVMTRef $ LLVMIdent "agda.data.base"

typeDataBasePtr :: LLVMType
typeDataBasePtr = LLVMPtr typeDataBase

refAllocData :: LLVMRef
refAllocData =
  LLVMGlobal
    {refName = LLVMIdent "agda.alloc.data", refType = LLVMFn {fnRet = typeDataBasePtr, fnParams = [LLVMSizedInt 64]}}

--- Value Struct ---
typeValue :: LLVMType
typeValue = LLVMTRef $ LLVMIdent "agda.struct.value"

typeValuePtr :: LLVMType
typeValuePtr = LLVMPtr typeValue

typeValueData :: LLVMType
typeValueData = LLVMTRef $ LLVMIdent "agda.struct.value.value"

typeValueDataPtr :: LLVMType
typeValueDataPtr = LLVMPtr typeValueData

typeValueFn :: LLVMType
typeValueFn = LLVMTRef $ LLVMIdent "agda.struct.value.fn"

typeValueFnPtr :: LLVMType
typeValueFnPtr = LLVMPtr typeValueFn

refAllocValue :: LLVMRef
refAllocValue =
  LLVMGlobal {refName = LLVMIdent "agda.alloc.value", refType = LLVMFn {fnRet = typeValuePtr, fnParams = []}}

--- Thunk Struct ---
typeThunk :: LLVMType
typeThunk = LLVMTRef $ LLVMIdent "agda.struct.thunk"

typeThunkPtr :: LLVMType
typeThunkPtr = LLVMPtr typeThunk

typeThunkEval :: LLVMType
typeThunkEval = LLVMTRef $ LLVMIdent "agda.struct.thunk.eval"

typeThunkEvalPtr :: LLVMType
typeThunkEvalPtr = LLVMPtr typeThunkEval

typeThunkValue :: LLVMType
typeThunkValue = LLVMTRef $ LLVMIdent "agda.struct.thunk.value"

typeThunkValuePtr :: LLVMType
typeThunkValuePtr = LLVMPtr typeThunkValue

refAllocThunk :: LLVMRef
refAllocThunk =
  LLVMGlobal {refName = LLVMIdent "agda.alloc.thunk", refType = LLVMFn {fnRet = typeThunkPtr, fnParams = []}}

--- Misc ---
refMain :: LLVMRef
refMain =
  LLVMGlobal {refName = LLVMIdent "agda.eval.main", refType = LLVMFn {fnRet = typeValuePtr, fnParams = [typeFnCreator]}}

refAppl0 :: LLVMRef
refAppl0 =
  LLVMGlobal
    {refName = LLVMIdent "agda.eval.appl.0", refType = LLVMFn {fnRet = typeValuePtr, fnParams = [typeThunkPtr]}}

typeFnCreator :: LLVMType
typeFnCreator = LLVMFn {fnRet = typeThunkPtr, fnParams = [typeVoidPtr]}

typeFnEvaluator :: LLVMType
typeFnEvaluator = LLVMFn {fnRet = typeValuePtr, fnParams = [typeVoidPtr]}

typeVoidPtr :: LLVMType
typeVoidPtr = LLVMPtr $ LLVMSizedInt 8
