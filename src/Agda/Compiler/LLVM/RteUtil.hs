module Agda.Compiler.LLVM.RteUtil where

import Agda.Compiler.LLVM.Syntax
import Agda.Compiler.LLVM.SyntaxUtil (llvmIdent)

i8 :: LLVMType
i8 = LLVMSizedInt 8

i64 :: LLVMType
i64 = LLVMSizedInt 64

i8Ptr :: LLVMType
i8Ptr = LLVMPtr i8

--- Data Base Struct ---
typeDataBase :: LLVMType
typeDataBase = LLVMTRef $ LLVMIdent "agda.data.base"

typeDataBasePtr :: LLVMType
typeDataBasePtr = LLVMPtr typeDataBase

refAllocData :: LLVMRef
refAllocData =
  LLVMGlobal
    { refName = LLVMIdent "agda.alloc.data"
    , refType = LLVMFn {fnRet = typeDataBasePtr, fnParams = [i64], fnVariadic = False}
    }

--- Value Struct ---
typeValueCommon :: String -> LLVMType
typeValueCommon v = LLVMTRef $ LLVMIdent $ "agda.struct.value." ++ v

typeValue :: LLVMType
typeValue = LLVMTRef $ LLVMIdent "agda.struct.value"

typeValuePtr :: LLVMType
typeValuePtr = LLVMPtr typeValue

typeValueData :: LLVMType
typeValueData = typeValueCommon "data"

typeValueDataPtr :: LLVMType
typeValueDataPtr = LLVMPtr typeValueData

typeValueFn :: LLVMType
typeValueFn = typeValueCommon "fn"

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
    , refType = LLVMFn {fnRet = typeThunkPtr, fnParams = [typeFramePtr, i64], fnVariadic = False}
    }

refRecordExtract :: LLVMRef
refRecordExtract =
  LLVMGlobal
    { refName = LLVMIdent "agda.record.extract"
    , refType = LLVMFn {fnRet = typeFramePtr, fnParams = [typeFramePtr, i64, typeThunkPtr], fnVariadic = False}
    }

--- Misc ---
refMain :: LLVMRef
refMain =
  LLVMGlobal
    { refName = LLVMIdent "agda.eval.main"
    , refType = LLVMFn {fnRet = typeValuePtr, fnParams = [LLVMPtr typeFnCreator], fnVariadic = False}
    }

refForce :: LLVMRef
refForce =
  LLVMGlobal
    { refName = LLVMIdent "agda.eval.force"
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
    , refType = LLVMFn {fnRet = i64, fnParams = [typeThunkPtr], fnVariadic = False}
    }

refCaseLitNat :: LLVMRef
refCaseLitNat =
  LLVMGlobal
    { refName = LLVMIdent "agda.eval.case.lit_nat"
    , refType = LLVMFn {fnRet = i64, fnParams = [typeThunkPtr], fnVariadic = False}
    }

typeFnCreator :: LLVMType
typeFnCreator = LLVMFn {fnRet = typeThunkPtr, fnParams = [typeFramePtr], fnVariadic = False}

mkCreatorFnSign :: LLVMIdent -> LLVMFnSign
mkCreatorFnSign ident =
  LLVMFnSign
    {fnName = ident, fnType = typeThunkPtr, fnArgs = [(typeFramePtr, llvmIdent "record")], fnArgsVariadic = False}

typeFnEvaluator :: LLVMType
typeFnEvaluator = LLVMFn {fnRet = typeValuePtr, fnParams = [typeFramePtr, typeThunkPtr], fnVariadic = False}

--- General Types for C Interfacing ---
typeVoidPtr :: LLVMType
typeVoidPtr = LLVMPtr $ LLVMSizedInt 8

refSysExit :: LLVMRef
refSysExit =
  LLVMGlobal {refName = LLVMIdent "exit", refType = LLVMFn {fnRet = LLVMVoid, fnParams = [i64], fnVariadic = False}}
