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
    { refName = LLVMIdent "agda.alloc.data"
    , refType = LLVMFn {fnRet = typeDataBasePtr, fnParams = [LLVMSizedInt 64], fnVariadic = False}
    }

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
  LLVMGlobal
    {refName = LLVMIdent "agda.alloc.value", refType = LLVMFn {fnRet = typeValuePtr, fnParams = [], fnVariadic = False}}

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
  LLVMGlobal
    {refName = LLVMIdent "agda.alloc.thunk", refType = LLVMFn {fnRet = typeThunkPtr, fnParams = [], fnVariadic = False}}

--- Stack ---
typeFrame :: LLVMType
typeFrame = LLVMTRef $ LLVMIdent "agda.struct.frame"

typeFramePtr :: LLVMType
typeFramePtr = LLVMPtr typeFrame

refRecordPushReplace :: LLVMRef
refRecordPushReplace =
  LLVMGlobal
    { refName = LLVMIdent "agda.record.push_replace"
    , refType = LLVMFn {fnRet = LLVMVoid, fnParams = [LLVMPtr typeFramePtr, typeThunkPtr], fnVariadic = False}
    }

refRecordGet :: LLVMRef
refRecordGet =
  LLVMGlobal
    { refName = LLVMIdent "agda.record.get"
    , refType = LLVMFn {fnRet = typeThunkPtr, fnParams = [typeFramePtr, LLVMSizedInt 64], fnVariadic = False}
    }

--- Misc ---
refMain :: LLVMRef
refMain =
  LLVMGlobal
    { refName = LLVMIdent "agda.eval.main"
    , refType = LLVMFn {fnRet = typeValuePtr, fnParams = [LLVMPtr typeFnCreator], fnVariadic = False}
    }

refAppl0 :: LLVMRef
refAppl0 =
  LLVMGlobal
    { refName = LLVMIdent "agda.eval.appl.0"
    , refType = LLVMFn {fnRet = typeValuePtr, fnParams = [typeThunkPtr], fnVariadic = False}
    }

refApplN :: LLVMRef
refApplN =
  LLVMGlobal
    { refName = LLVMIdent "agda.eval.appl.n"
    , refType = LLVMFn {fnRet = typeValuePtr, fnParams = [typeThunkPtr], fnVariadic = True}
    }

refCaseData :: LLVMRef
refCaseData =
  LLVMGlobal
    { refName = LLVMIdent "agda.eval.case.data"
    , refType = LLVMFn {fnRet = LLVMSizedInt 64, fnParams = [typeThunkPtr], fnVariadic = False}
    }

typeFnCreator :: LLVMType
typeFnCreator = LLVMFn {fnRet = typeThunkPtr, fnParams = [typeFramePtr], fnVariadic = False}

typeFnEvaluator :: LLVMType
typeFnEvaluator = LLVMFn {fnRet = typeValuePtr, fnParams = [typeFramePtr, typeThunkPtr], fnVariadic = False}

typeVoidPtr :: LLVMType
typeVoidPtr = LLVMPtr $ LLVMSizedInt 8
