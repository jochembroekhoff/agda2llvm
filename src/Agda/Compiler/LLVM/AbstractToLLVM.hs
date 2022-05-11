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
  aToLlvm (AIdentRaw identRaw) = LLVMIdent identRaw

instance AToLlvm AEntry [LLVMEntry] where
  aToLlvm (AEntryThunk ident thunk) = thunkConstructor : maybeToList thunkEvaluator
    where
      (thunkConstructor, thunkEvaluator) = aToLlvm (ident, thunk)
  aToLlvm (AEntryDirect ident push body) = [aToLlvm (ident, push, body)]
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
      thunkEvaluator = aToLlvm (bodyIdent, False, body)
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
              { storeSrc = LLVMLit $ LLVMNull typeFramePtr
              , storeDest = LLVMLocal (llvmIdent "thunk_eval_record") (LLVMPtr typeFramePtr)
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
           , llvmRecord "thunk_value_ptr" $
             LLVMGetElementPtr
               { elemBase = typeThunkValue
               , elemSrc = LLVMLocal (llvmIdent "thunk_value") typeThunkValuePtr
               , elemIndices = [0, 1]
               }
           , llvmDiscard $
             LLVMStore
               { storeSrc = LLVMRef $ LLVMLocal (llvmIdent "v") typeValuePtr
               , storeDest = LLVMLocal (llvmIdent "thunk_value_ptr") (LLVMPtr typeValuePtr)
               }
           ])

thunkCreatorTemplate :: AIdent -> Bool -> [(Maybe LLVMIdent, LLVMInstruction)] -> LLVMEntry
thunkCreatorTemplate ident isEval instructions =
  LLVMFnDefn
    { fnSign = LLVMFnSign {fnName = aToLlvm ident, fnType = typeThunkPtr, fnArgs = [(typeFramePtr, llvmIdent "record")]}
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

instance AToLlvm (AIdent, Bool, ABody) LLVMEntry where
  aToLlvm (ident, push, AMkValue value) = bodyTemplate ident push (aToLlvm value)
  -- appl(0)
  aToLlvm (ident, push, AAppl (AExt subj) []) =
    bodyTemplate
      ident
      push
    -- let the callee create its thunk
      [ llvmRecord "appl" $
        LLVMCall {callRef = LLVMGlobal (aToLlvm subj) typeFnCreator, callArgs = [LLVMLit $ LLVMNull typeFramePtr]}
    -- call the application-0 helper function from the runtime
      , llvmRecord "v" $ LLVMCall {callRef = refAppl0, callArgs = [LLVMRef $ LLVMLocal (llvmIdent "appl") typeThunkPtr]}
      ]
  -- appl(n)
  aToLlvm (ident, push, AAppl subj args) = bodyTemplate ident push (instrSubj ++ instrArgs ++ instrDoApply)
    where
      (refSubj, instrSubj) = aToLlvm (subj, 0 :: Int)
      args' = zipWith (curry aToLlvm) args ([1 ..] :: [Int])
      refsArgs = map fst args'
      instrArgs = concatMap snd args'
      -- | terminator arg signals the end of the variadic arguments
      terminatorArg = LLVMLit $ LLVMNull typeThunkPtr
      instrDoApply = [llvmRecord "v" $ LLVMCall {callRef = refApplN, callArgs = refSubj : refsArgs ++ [terminatorArg]}]

bodyTemplate :: AIdent -> Bool -> [(Maybe LLVMIdent, LLVMInstruction)] -> LLVMEntry
bodyTemplate ident push instructions =
  LLVMFnDefn
    { fnSign =
        LLVMFnSign
          { fnName = aToLlvm ident
          , fnType = typeValuePtr
          , fnArgs = [(typeFramePtr, llvmIdent recordParamName), (typeThunkPtr, llvmIdent "arg")]
          }
    , body = [LLVMBlock "begin" (pushTheArg ++ instructions ++ [returnTheValue])]
    }
  where
    recordParamName =
      if push
        then "record_orig"
        else "record"
    pushTheArg =
      if push
      -- allocate a working pointer-pointer to replace the record
        then [ llvmRecord "record_work_ptr" $ LLVMAlloca typeFramePtr
      -- load the pointer from the fn argument into the work pointer
             , llvmDiscard $
               LLVMStore
                 { storeSrc = LLVMRef $ LLVMLocal (llvmIdent "record_orig") typeFramePtr
                 , storeDest = LLVMLocal (llvmIdent "record_work_ptr") (LLVMPtr typeFramePtr)
                 }
      -- ask the runtime to replace the pointer with an updated record which contains the argument on top
             , llvmDiscard $
               LLVMCall
                 { callRef = refRecordPushReplace
                 , callArgs =
                     [ LLVMRef $ LLVMLocal (llvmIdent "record_work_ptr") (LLVMPtr typeFramePtr)
                     , LLVMRef $ LLVMLocal (llvmIdent "arg") typeThunkPtr
                     ]
                 }
      -- load the result in the work pointer into a final pointer for later use in the body
             , llvmRecord "record" $
               LLVMLoad
                 {loadType = typeFramePtr, loadSrc = LLVMLocal (llvmIdent "record_work_ptr") (LLVMPtr typeFramePtr)}
             ]
        else []
    returnTheValue = llvmDiscard $ LLVMRet $ Just $ LLVMRef $ LLVMLocal (llvmIdent "v") typeValuePtr

instance AToLlvm (AArg, Int) (LLVMValue, [(Maybe LLVMIdent, LLVMInstruction)]) where
  aToLlvm (AExt ident, argIdx) =
    ( LLVMRef $ LLVMLocal (llvmIdent local) typeThunkPtr
    , [ llvmRecord local $
        LLVMCall {callRef = LLVMGlobal (aToLlvm ident) typeFnCreator, callArgs = [LLVMLit $ LLVMNull typeFramePtr]}
      ])
    where
      local = "arg-" ++ show argIdx
  aToLlvm (ARecord idx, argIdx) =
    ( LLVMRef $ LLVMLocal (llvmIdent local) typeThunkPtr
    , [ llvmRecord local $
        LLVMCall
          { callRef = refRecordGet
          , callArgs = [LLVMRef $ LLVMLocal (llvmIdent "record") typeFramePtr, LLVMLit $ LLVMInt (LLVMSizedInt 64) idx]
          }
      ])
    where
      local = "arg-" ++ show argIdx

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
            { storeSrc = LLVMLit $ LLVMNull typeFramePtr
            , storeDest = LLVMLocal (llvmIdent "v_fn_record") (LLVMPtr typeFramePtr)
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
