module Agda.Compiler.LLVM.AbstractToLLVM where

import Agda.Compiler.LLVM.ASyntax
import Agda.Compiler.LLVM.RteUtil
import Agda.Compiler.LLVM.Syntax
import Agda.Compiler.LLVM.SyntaxUtil
import Agda.Utils.Maybe (maybeToList)

class AToLlvm a b where
  aToLlvm :: a -> b

instance AToLlvm AIdent LLVMIdent where
  aToLlvm (AIdent ident) = llvmIdent ident

instance AToLlvm AEntry [LLVMEntry] where
  aToLlvm (AEntryThunk ident thunk) = thunkConstructor : maybeToList thunkEvaluator
    where
      (thunkConstructor, thunkEvaluator) = aToLlvm (ident, thunk)
  aToLlvm (AEntryDirect ident body) = [aToLlvm (ident, body)]
  aToLlvm (AEntryMain mainRef) =
    [ LLVMFnDefn
        { fnSign = llvmMainSignature
        , body =
            [ LLVMBlock
                "begin"
                [ llvmDiscard $
                  LLVMCall
                    {callRef = refMain, callArgs = [LLVMRef $ LLVMGlobal (aToLlvm mainRef) (LLVMPtr typeFnCreator)]}
                , llvmDiscard $ LLVMRet $ Just $ LLVMLit $ LLVMInt (LLVMSizedInt 64) 0
                ]
            ]
        }
    ]

instance AToLlvm (AIdent, AThunk) (LLVMEntry, Maybe LLVMEntry) where
  aToLlvm (ident, AThunkDelay body) = (thunkConstructor, Just thunkEvaluator)
    where
      bodyIdent = ident <> AIdent "--body"
      thunkEvaluator = aToLlvm (bodyIdent, body)
      thunkConstructor =
        thunkCreatorTemplate
          ident
          False
        -- configure non-evaluated setting
          [ llvmRecord "thunk_eval" $
            LLVMBitcast {bitcastFrom = LLVMLocal (llvmIdent "thunk_raw") typeThunkPtr, bitcastTo = typeThunkEvalPtr}
        -- store function pointer
          , llvmRecord "thunk_eval_ptr" $
            LLVMGetElementPtr
              { elemBase = typeThunkEval
              , elemSrc = LLVMLocal (llvmIdent "thunk_eval") typeThunkEvalPtr
              , elemIndices = [0, 1, 0]
              }
          , llvmDiscard $
            LLVMStore
              { storeSrc = LLVMRef $ LLVMGlobal (aToLlvm bodyIdent) (LLVMPtr typeFnEvaluator)
              , storeDest = LLVMLocal (llvmIdent "thunk_eval_ptr") (LLVMPtr $ LLVMPtr typeFnEvaluator)
              }
        -- store function record
          , llvmRecord "thunk_eval_record" $
            LLVMGetElementPtr
              { elemBase = typeThunkEval
              , elemSrc = LLVMLocal (llvmIdent "thunk_eval") typeThunkEvalPtr
              , elemIndices = [0, 1, 1]
              }
          , llvmDiscard $
            LLVMStore
              { storeSrc = LLVMLit $ LLVMNull typeVoidPtr
              , storeDest = LLVMLocal (llvmIdent "thunk_eval_record") (LLVMPtr typeVoidPtr)
              }
          ]
  aToLlvm (ident, AThunkValue value) = (thunkConstructor, Nothing)
    where
      valueCreateInstructions = aToLlvm value
      thunkConstructor =
        thunkCreatorTemplate
          ident
          True
          (valueCreateInstructions ++
        -- configure non-evaluated setting
           [ llvmRecord "thunk_value" $
             LLVMBitcast {bitcastFrom = LLVMLocal (llvmIdent "thunk_raw") typeThunkPtr, bitcastTo = typeThunkValuePtr}
        -- copy value pointer into the thunk
           , undefined {- TODO: %v -> %thunk_value[0][1] -}
           ])

thunkCreatorTemplate :: AIdent -> Bool -> [(Maybe LLVMIdent, LLVMInstruction)] -> LLVMEntry
thunkCreatorTemplate ident isEval instructions =
  LLVMFnDefn
    { fnSign = LLVMFnSign {fnName = aToLlvm ident, fnType = typeThunkPtr, fnArgs = [(typeVoidPtr, llvmIdent "record")]}
    , body = [LLVMBlock "begin" (constructThunkHolder ++ instructions ++ [returnThunkHolder])]
    }
  where
    constructThunkHolder
        -- construct a new instance
     =
      [ llvmRecord "thunk_raw" $ LLVMCall {callRef = refAllocThunk, callArgs = []}
      -- store eval flag
      , llvmRecord "thunk_flag" $
        LLVMGetElementPtr
          {elemBase = typeThunk, elemSrc = LLVMLocal (llvmIdent "thunk_raw") typeThunkPtr, elemIndices = [0, 0]}
      , llvmRecord "thunk_flag_bool" $ LLVMZext {zextFrom = LLVMLit $ LLVMBool isEval, zextTo = LLVMSizedInt 64}
      , llvmDiscard $
        LLVMStore
          { storeSrc = LLVMRef $ LLVMLocal (llvmIdent "thunk_flag_bool") (LLVMSizedInt 64)
          , storeDest = LLVMLocal (llvmIdent "thunk_flag") (LLVMPtr $ LLVMSizedInt 64)
          }
      ]
    returnThunkHolder = llvmDiscard $ LLVMRet $ Just $ LLVMRef $ LLVMLocal (llvmIdent "thunk_raw") typeThunkPtr

instance AToLlvm (AIdent, ABody) LLVMEntry where
  aToLlvm (ident, AMkValue value) = bodyTemplate ident (aToLlvm value)
  aToLlvm (ident, AAppl subj []) =
    bodyTemplate
      ident
    -- let the callee create its thunk
      [ llvmRecord "appl" $
        LLVMCall {callRef = LLVMGlobal (aToLlvm subj) typeFnCreator, callArgs = [LLVMLit $ LLVMNull typeVoidPtr]}
    -- call the application-0 helper function from the runtime
      , llvmRecord "v" $ LLVMCall {callRef = refAppl0, callArgs = [LLVMRef $ LLVMLocal (llvmIdent "appl") typeThunkPtr]}
      ]
  aToLlvm (ident, AAppl subj args) = undefined

bodyTemplate :: AIdent -> [(Maybe LLVMIdent, LLVMInstruction)] -> LLVMEntry
bodyTemplate ident instructions =
  LLVMFnDefn
    { fnSign = LLVMFnSign {fnName = aToLlvm ident, fnType = typeValuePtr, fnArgs = [(typeVoidPtr, llvmIdent "record")]}
    , body = [LLVMBlock "begin" (instructions ++ [returnTheValue])]
    }
  where
    returnTheValue = llvmDiscard $ LLVMRet $ Just $ LLVMRef $ LLVMLocal (llvmIdent "v") typeValuePtr

instance AToLlvm AValue [(Maybe LLVMIdent, LLVMInstruction)] where
  aToLlvm (AValueData idx kase arity) = createData ++ populateData ++ createValue ++ populateValue
    where
      dataBaseSize = 16
      dataSize = dataBaseSize + 8 * arity
      createData
        -- initialize empty data base
       =
        [ llvmRecord "data_base" $
          LLVMCall {callRef = refAllocData, callArgs = [LLVMLit $ LLVMInt (LLVMSizedInt 64) dataSize]}
        -- store idx
        , llvmRecord "data_base_idx" $
          LLVMGetElementPtr
            {elemBase = typeDataBase, elemSrc = LLVMLocal (llvmIdent "data_base") typeDataBasePtr, elemIndices = [0, 0]}
        , llvmDiscard $
          LLVMStore
            { storeSrc = LLVMLit $ LLVMInt (LLVMSizedInt 64) idx
            , storeDest = LLVMLocal (llvmIdent "data_base_idx") (LLVMPtr $ LLVMSizedInt 64)
            }
        -- store case
        , llvmRecord "data_base_case" $
          LLVMGetElementPtr
            {elemBase = typeDataBase, elemSrc = LLVMLocal (llvmIdent "data_base") typeDataBasePtr, elemIndices = [0, 1]}
        , llvmDiscard $
          LLVMStore
            { storeSrc = LLVMLit $ LLVMInt (LLVMSizedInt 64) kase
            , storeDest = LLVMLocal (llvmIdent "data_base_case") (LLVMPtr $ LLVMSizedInt 64)
            }
        ]
      populateData
        -- TODO: only support 0-arity constructors
       = []
      createValue = createValueTagged 1
      populateValue
        -- copy the data holder pointer into the value holder
       =
        [ llvmRecord "v_data" $
          LLVMBitcast {bitcastFrom = LLVMLocal (llvmIdent "v") typeValuePtr, bitcastTo = typeValueDataPtr}
        , llvmRecord "v_data_content" $
          LLVMGetElementPtr
            {elemBase = typeValueData, elemSrc = LLVMLocal (llvmIdent "v_data") typeValueDataPtr, elemIndices = [0, 1]}
        , llvmDiscard $
          LLVMStore
            { storeSrc = LLVMRef $ LLVMLocal (llvmIdent "data_base") typeDataBasePtr
            , storeDest = LLVMLocal (llvmIdent "v_data_content") (LLVMPtr typeDataBasePtr)
            }
        ]
  aToLlvm (AValueFn fnIdent) = createValue ++ populateValue
    where
      createValue = createValueTagged 0
      populateValue
        -- store the function pointer + record into the value holder
       =
        [ llvmRecord "v_fn" $
          LLVMBitcast {bitcastFrom = LLVMLocal (llvmIdent "v") typeValuePtr, bitcastTo = typeValueFnPtr}
          -- store function pointer
        , llvmRecord "v_fn_ptr" $
          LLVMGetElementPtr
            {elemBase = typeValueFn, elemSrc = LLVMLocal (llvmIdent "v_fn") typeValueFnPtr, elemIndices = [0, 1, 0]}
        , llvmDiscard $
          LLVMStore
            { storeSrc = LLVMRef $ LLVMGlobal (aToLlvm fnIdent) (LLVMPtr typeFnEvaluator)
            , storeDest = LLVMLocal (llvmIdent "v_fn_ptr") (LLVMPtr $ LLVMPtr typeFnEvaluator)
            }
          -- store function record
        , llvmRecord "v_fn_record" $
          LLVMGetElementPtr
            {elemBase = typeValueFn, elemSrc = LLVMLocal (llvmIdent "v_fn") typeValueFnPtr, elemIndices = [0, 1, 1]}
        , llvmDiscard $
          LLVMStore
            { storeSrc = LLVMLit $ LLVMNull typeVoidPtr
            , storeDest = LLVMLocal (llvmIdent "v_fn_record") (LLVMPtr typeVoidPtr)
            }
        ]

-- | Create some instructions that populate @%v@ with a fresh value struct.
--   The given tag value is stored, the other fields are left uninitialized.
createValueTagged :: Int -> [(Maybe LLVMIdent, LLVMInstruction)]
createValueTagged tag
  -- initialize/alloc empty value holder
 =
  [ llvmRecord "v" $ LLVMCall {callRef = refAllocValue, callArgs = []}
  -- configure the tag as is given
  , llvmRecord "v_tag" $
    LLVMGetElementPtr {elemBase = typeValue, elemSrc = LLVMLocal (llvmIdent "v") typeValuePtr, elemIndices = [0, 0]}
  , llvmDiscard $
    LLVMStore
      { storeSrc = LLVMLit $ LLVMInt (LLVMSizedInt 64) tag
      , storeDest = LLVMLocal (llvmIdent "v_tag") (LLVMPtr $ LLVMSizedInt 64)
      }
  ]
