module Agda.Compiler.LLVM.AbstractToLLVM where

import Agda.Compiler.Backend (EvaluationStrategy(EagerEvaluation, LazyEvaluation), __IMPOSSIBLE_VERBOSE__)
import Agda.Compiler.LLVM.ASyntax
import Agda.Compiler.LLVM.RteUtil
import Agda.Compiler.LLVM.Syntax hiding (litType)
import Agda.Compiler.LLVM.SyntaxUtil
import Agda.Compiler.LLVM.Tables
import Agda.Syntax.Literal (Literal(..))
import Agda.Utils.Maybe (maybeToList)
import Agda.Utils.Tuple
import Data.Char (ord)
import Text.Printf (printf)

class AToLlvm a b where
  aToLlvm :: a -> b

instance AToLlvm AIdent LLVMIdent where
  aToLlvm (AIdent ident) = llvmIdent ident
  aToLlvm (AIdentRaw identRaw) = LLVMIdent identRaw

instance AToLlvm (EvaluationStrategy, AEntry) [LLVMEntry]
  -- TODO: respect the private flag (should correspond to LLVM private modifier)
                                                                                 where
  aToLlvm (evalStrat, AEntryThunk ident private thunk) = thunkConstructor : thunkEvaluator
    where
      (thunkConstructor, thunkEvaluator) = aToLlvm (ident, private, thunk, evalStrat)
  aToLlvm (_, AEntryDirect ident push body) = aToLlvm ((ident, False, push), body)
  aToLlvm (_, AEntryMain mainRef) =
    [ LLVMFnDefn
        { fnModifiers = []
        , fnSign = llvmMainSignature
        , body =
            [ LLVMBlock
                (llvmIdent "begin")
                [ llvmDiscard $
                  LLVMCall
                    {callRef = refMain, callArgs = [LLVMRef $ LLVMGlobal (aToLlvm mainRef) (LLVMPtr typeFnCreator)]}
                , llvmDiscard $ LLVMRet $ Just $ LLVMLit $ LLVMInt 64 0
                ]
            ]
        }
    ]
  aToLlvm (_, AEntryAlias ident alias) =
    [ LLVMFnDefn
        { fnModifiers = []
        , fnSign = mkCreatorFnSign $ aToLlvm ident
        , body =
            [ LLVMBlock
                (llvmIdent "begin")
                [ llvmRecord "res" $
                  LLVMCall
                    { callRef = LLVMGlobal (aToLlvm alias) typeFnCreator
                    , callArgs = [LLVMRef $ LLVMLocal (llvmIdent "record") typeFramePtr]
                    }
                , llvmDiscard $ LLVMRet $ Just $ LLVMRef $ LLVMLocal (llvmIdent "res") typeThunkPtr
                ]
            ]
        }
    ]

-- | Precompute a static LLVM value for a thunk, if possible.
--   This is technically possible for all thunks, but it is only
--   implemented for value thunks.
--   Delayed thunks that produce a value should already have been optimized out.
--   Additionally, this is only safe to do for public thunks,
--   as they are guaranteed to not rely on the current record instance.
precomputePublicThunk :: AThunk -> Maybe (LLVMValue, [LLVMEntry])
precomputePublicThunk (AThunkValue v) =
  Just $
  case v of
    AValueData idx kase arity ->
      let dataBaseIdent = LLVMIdent $ printf "static-%d.%d" idx kase
          dataBaseValue =
            LLVMLit $
            LLVMStructInst False [LLVMLit $ LLVMInt 64 idx, LLVMLit $ LLVMInt 64 kase, LLVMLit $ LLVMNull typeFramePtr]
       in ( LLVMLit $
            LLVMStructInst
              False
              [ LLVMLit $ LLVMInt 64 1 -- tag=1 : data
              , LLVMRef $ LLVMGlobal dataBaseIdent (LLVMPtr $ llvmValueType dataBaseValue)
              ]
          , [LLVMConstant {constName = dataBaseIdent, constValue = dataBaseValue}])
    AValueFn fnIdent ->
      ( LLVMLit $
        LLVMStructInst
          False
          [ LLVMLit $ LLVMInt 64 0 -- tag=0 : fn
          , LLVMRef $ LLVMGlobal (aToLlvm fnIdent) (LLVMPtr typeFnEvaluator)
          , LLVMLit $ LLVMNull typeFramePtr
          ]
      , [])
    AValueLit lit ->
      ( LLVMLit $
        LLVMStructInst
          False
          [ LLVMLit $ LLVMInt 64 $ litTag lit -- tag= derived from the literal
          , LLVMLit $ litLit lit -- value is the literal itself
          ]
      , [])
precomputePublicThunk _ = Nothing

instance AToLlvm (AIdent, Bool, AThunk, EvaluationStrategy) (LLVMEntry, [LLVMEntry]) where
  aToLlvm (ident, private@False, thunk, LazyEvaluation)
    | Just (precomputedValue, aux) <- precomputePublicThunk thunk =
      mapSnd (++ aux) $ thunkCreatorTemplate (ident, private, True) (Right precomputedValue)
  aToLlvm (ident, private, AThunkDelay body, LazyEvaluation) = (thunkConstructor, thunkEvaluator ++ extraEntries)
    where
      bodyIdent = ident <> AIdent "--body"
      thunkEvaluator = aToLlvm ((bodyIdent, True, False), body)
      (thunkConstructor, extraEntries) =
        thunkCreatorTemplate
          ( ident
          , private
            -- configure non-evaluated setting
          , False)
          (Left
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
                 { storeSrc = LLVMRef $ LLVMLocal (llvmIdent "record") typeFramePtr
                 , storeDest = LLVMLocal (llvmIdent "thunk_eval_record") (LLVMPtr typeFramePtr)
                 }
             ])
  aToLlvm (ident, private, AThunkDelay body, EagerEvaluation) = (thunkConstructor, thunkEvaluator ++ extraEntries)
    where
      bodyIdent = ident <> AIdent "--body-strict"
      thunkEvaluator = aToLlvm ((bodyIdent, True, False), body)
      (thunkConstructor, extraEntries) =
        thunkCreatorTemplate
          (ident, private, True)
          (Left
             [ llvmRecord "thunk_value" $
               LLVMBitcast {bitcastFrom = LLVMLocal (llvmIdent "thunk_raw") typeThunkPtr, bitcastTo = typeThunkValuePtr}
             , llvmRecord "thunk_value_ptr" $
               LLVMGetElementPtr
                 { elemBase = typeThunkValue
                 , elemSrc = LLVMLocal (llvmIdent "thunk_value") typeThunkValuePtr
                 , elemIndices = [0, 1]
                 }
             , llvmRecord "v" $
               LLVMCall
                 { callRef = LLVMGlobal (aToLlvm bodyIdent) typeFnEvaluator
                 , callArgs = [LLVMRef $ LLVMLocal (llvmIdent "record") typeFramePtr, LLVMLit $ LLVMNull typeThunkPtr]
                 }
             , llvmDiscard $
               LLVMStore
                 { storeSrc = LLVMRef $ LLVMLocal (llvmIdent "v") typeValuePtr
                 , storeDest = LLVMLocal (llvmIdent "thunk_value_ptr") (LLVMPtr typeValuePtr)
                 }
             ])
  aToLlvm (ident, private, AThunkValue value, _) = (thunkConstructor, extraEntries)
    where
      valueCreateInstructions = aToLlvm value
      (thunkConstructor, extraEntries) =
        thunkCreatorTemplate
          (ident, private, True)
          (Left $
           valueCreateInstructions ++
        -- configure already-evaluated setting
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

thunkCreatorTemplate ::
     (AIdent, Bool, Bool) -> Either [(Maybe LLVMIdent, LLVMInstruction)] LLVMValue -> (LLVMEntry, [LLVMEntry])
thunkCreatorTemplate (ident, isPrivate, isEval) (Left instructions) =
  ( LLVMFnDefn
      { fnModifiers = [LLVMPrivate | isPrivate]
      , fnSign = mkCreatorFnSign $ aToLlvm ident
      , body = [LLVMBlock (llvmIdent "begin") (constructThunkHolder ++ instructions ++ [returnThunkHolder])]
      }
  , [])
  where
    constructThunkHolder
        -- construct a new instance
     =
      [ llvmRecord "thunk_raw" $ LLVMCall {callRef = refAllocThunk, callArgs = []}
      -- store eval flag
      , llvmRecord "thunk_flag" $
        LLVMGetElementPtr
          {elemBase = typeThunk, elemSrc = LLVMLocal (llvmIdent "thunk_raw") typeThunkPtr, elemIndices = [0, 0]}
      , llvmRecord "thunk_flag_bool" $ LLVMZext {zextFrom = LLVMLit $ LLVMBool isEval, zextTo = i64}
      , llvmDiscard $
        LLVMStore
          { storeSrc = LLVMRef $ LLVMLocal (llvmIdent "thunk_flag_bool") i64
          , storeDest = LLVMLocal (llvmIdent "thunk_flag") (LLVMPtr i64)
          }
      ]
    returnThunkHolder = llvmDiscard $ LLVMRet $ Just $ LLVMRef $ LLVMLocal (llvmIdent "thunk_raw") typeThunkPtr
thunkCreatorTemplate (ident, isPrivate, True) (Right value) =
  ( LLVMFnDefn
      { fnModifiers = [LLVMPrivate | isPrivate]
      , fnSign = mkCreatorFnSign $ aToLlvm ident
      , body = [LLVMBlock (llvmIdent "begin") instructions]
      }
  , [valueGlobalizer, thunkWrapperGlobalizer])
  where
    identGlobalizer = aToLlvm $ ident <> AIdent "--thunk_v"
    identWrapper = aToLlvm $ ident <> AIdent "--thunk_v_wrapper"
    valueGlobalizer = LLVMConstant identGlobalizer value
    thunkWrapper =
      LLVMLit $
      LLVMStructInst
        False
        [ LLVMLit $ LLVMInt 64 1 -- evaluated flag set to true
        , LLVMRef $ LLVMGlobal identGlobalizer (LLVMPtr $ llvmValueType value)
        ]
    thunkWrapperGlobalizer = LLVMConstant identWrapper thunkWrapper
    instructions =
      [ llvmRecord "thunk_generic" $
        LLVMBitcast
          {bitcastFrom = LLVMGlobal identWrapper (LLVMPtr $ llvmValueType thunkWrapper), bitcastTo = typeThunkPtr}
      , llvmDiscard $ LLVMRet $ Just $ LLVMRef $ LLVMLocal (llvmIdent "thunk_generic") typeThunkPtr
      ]
thunkCreatorTemplate (_, _, False) _ = undefined

instance AToLlvm ((AIdent, Bool, Bool), ABody) [LLVMEntry] where
  aToLlvm (cfg@(ident, _, push), AMkValue value) = pure $ bodyTemplate cfg (aToLlvm value)
  -- appl(0)
  aToLlvm (cfg@(ident, _, push), AAppl (AExt subj) []) =
    pure $
    bodyTemplate
      cfg
    -- let the callee create its thunk
      [ llvmRecord "appl" $
        LLVMCall {callRef = LLVMGlobal (aToLlvm subj) typeFnCreator, callArgs = [LLVMLit $ LLVMNull typeFramePtr]}
    -- call the application-0 helper function from the runtime
      , llvmRecord "v" $ LLVMCall {callRef = refForce, callArgs = [LLVMRef $ LLVMLocal (llvmIdent "appl") typeThunkPtr]}
      ]
  -- appl(n)
  aToLlvm (cfg@(ident, _, push), AAppl subj args) = pure $ bodyTemplate cfg (instrSubj ++ instrArgs ++ instrDoApply)
    where
      (refSubj, instrSubj) = aToLlvm (subj, 0 :: Int)
      args' = zipWith (curry aToLlvm) args ([1 ..] :: [Int])
      refsArgs = map fst args'
      instrArgs = concatMap snd args'
      -- | terminator arg signals the end of the variadic arguments
      terminatorArg = LLVMLit $ LLVMNull typeThunkPtr
      instrDoApply = [llvmRecord "v" $ LLVMCall {callRef = refApplN, callArgs = refSubj : refsArgs ++ [terminatorArg]}]
  aToLlvm (cfg@(ident, _, push), ACase (ARecordIdx subj) (AAData alts) fallback) =
    bodyTemplateBasic cfg primaryBody branchBlocks : fallbackEntries ++ altEntries
    where
      (fallbackIdentifier:altIdentifiers) = map (\i -> ident <> AIdent ("--case-" ++ show i)) [0 ..]
      fallbackEntries = aToLlvm ((fallbackIdentifier, True, False), fallback)
      altEntries =
        concat $ zipWith (\ident (_, _, altBody) -> aToLlvm ((ident, True, False), altBody)) altIdentifiers alts
      (fallbackEntry, switchEntries, branchBlocks) =
        caseStuffTemp (zip3 (map fst3 alts) (map snd3 alts) altIdentifiers) fallbackIdentifier
      primaryBody
          -- load the case subject ("scrutinee")
       =
        [ llvmRecord "case_subj_thunk" $
          LLVMCall
            { callRef = refRecordGet
            , callArgs = [LLVMRef $ LLVMLocal (llvmIdent "record") typeFramePtr, LLVMLit $ LLVMInt 64 subj]
            }
          -- call runtime to obtain identification info (data id)
        , llvmRecord "data_id" $
          LLVMCall {callRef = refCaseData, callArgs = [LLVMRef $ LLVMLocal (llvmIdent "case_subj_thunk") typeThunkPtr]}
          -- switch on the data id
        , llvmDiscard $
          LLVMSwitch
            { switchSubj = LLVMRef $ LLVMLocal (llvmIdent "data_id") i64
            , switchDefault = fallbackEntry
            , switchBranches = switchEntries
            }
        ]
  aToLlvm (cfg@(ident, _, push), ACase (ARecordIdx subj) (AANat alts) fallback) =
    bodyTemplateBasic cfg primaryBody branchBlocks : fallbackEntries ++ altEntries
    where
      (fallbackIdentifier:altIdentifiers) = map (\i -> ident <> AIdent ("--case-" ++ show i)) [0 ..]
      fallbackEntries = aToLlvm ((fallbackIdentifier, True, False), fallback)
      altEntries = concat $ zipWith (\ident (_, altBody) -> aToLlvm ((ident, True, False), altBody)) altIdentifiers alts
      (fallbackEntry, switchEntries, branchBlocks) =
        caseStuffTemp2 (zip (map fst alts) altIdentifiers) fallbackIdentifier
      primaryBody
          -- load the case subject ("scrutinee")
       =
        [ llvmRecord "case_subj_thunk" $
          LLVMCall
            { callRef = refRecordGet
            , callArgs = [LLVMRef $ LLVMLocal (llvmIdent "record") typeFramePtr, LLVMLit $ LLVMInt 64 subj]
            }
          -- call runtime to obtain the underlying value
        , llvmRecord "lit_nat_value" $
          LLVMCall
            {callRef = refCaseLitNat, callArgs = [LLVMRef $ LLVMLocal (llvmIdent "case_subj_thunk") typeThunkPtr]}
          -- switch on the data id
        , llvmDiscard $
          LLVMSwitch
            { switchSubj = LLVMRef $ LLVMLocal (llvmIdent "lit_nat_value") i64
            , switchDefault = fallbackEntry
            , switchBranches = switchEntries
            }
        ]
  aToLlvm (cfg@(ident, private, _), AError errorText) =
    pure $ bodyTemplate (ident, private, False) (assignNull "v" "vv")

bodyTemplateBasic :: (AIdent, Bool, Bool) -> [(Maybe LLVMIdent, LLVMInstruction)] -> [LLVMBlock] -> LLVMEntry
bodyTemplateBasic (ident, private, push) beginInstructions blocks =
  LLVMFnDefn
    { fnModifiers = [LLVMPrivate | private]
    , fnSign =
        LLVMFnSign
          { fnName = aToLlvm ident
          , fnType = typeValuePtr
          , fnArgs = [(typeFramePtr, llvmIdent recordParamName), (typeThunkPtr, llvmIdent "arg")]
          , fnArgsVariadic = False
          }
    , body = blockBegin : blocks
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
    blockBegin = LLVMBlock (llvmIdent "begin") (pushTheArg ++ beginInstructions)

bodyTemplate :: (AIdent, Bool, Bool) -> [(Maybe LLVMIdent, LLVMInstruction)] -> LLVMEntry
bodyTemplate cfg instructions = bodyTemplateBasic cfg (instructions ++ [returnTheValue]) []
  where
    returnTheValue = llvmDiscard $ LLVMRet $ Just $ LLVMRef $ LLVMLocal (llvmIdent "v") typeValuePtr

caseStuffTemp :: [(AIdent, Int, AIdent)] -> AIdent -> (LLVMIdent, [(LLVMLit, LLVMIdent)], [LLVMBlock])
caseStuffTemp cases fallback = (labelDefault, cases''1, blockDefault : cases''2)
  where
    fnBlock :: String -> Int -> AIdent -> LLVMIdent -> LLVMBlock
    fnBlock varSuffix arity ident lbl =
      LLVMBlock
        lbl
        (pushRecord ++
         [ llvmRecord var $
           LLVMCall
             { callRef = LLVMGlobal {refName = aToLlvm ident, refType = typeFnEvaluator}
             , callArgs = [LLVMRef $ LLVMLocal (llvmIdent recordVar) typeFramePtr, LLVMLit $ LLVMNull typeThunkPtr]
             }
         , llvmDiscard $ LLVMRet $ Just $ LLVMRef $ LLVMLocal (llvmIdent var) typeValuePtr
         ])
      where
        var = "v-" ++ varSuffix
        recordVar =
          if arity == 0
            then "record"
            else "record_extracted-" ++ varSuffix
        pushRecord =
          case arity of
            0 -> []
            n ->
              [ llvmRecord recordVar $
                LLVMCall
                  { callRef = refRecordExtract
                  , callArgs =
                      [ LLVMRef $ LLVMLocal (llvmIdent "record") typeFramePtr
                      , LLVMLit $ LLVMInt 64 arity
                      , LLVMRef $ LLVMLocal (llvmIdent "case_subj_thunk") typeThunkPtr
                      ]
                  }
              ]
    fn :: Int -> (AIdent, Int, AIdent) -> ((LLVMLit, LLVMIdent), LLVMBlock)
    fn i (ctorIdent, arity, destIdent) = ((LLVMInt 64 dataId, lbl), block)
      where
        dataId = uncurry (+) $ computeCtorIdent ctorIdent
        lbl = llvmIdent $ "case-" ++ show i
        block = fnBlock (show i) arity destIdent lbl
    -- calculate output for the fallback case
    labelDefault = llvmIdent "case-default"
    blockDefault = fnBlock "default" 0 fallback labelDefault
    -- calculate output for the normal cases
    cases' = zipWith fn [0 ..] cases
    cases''1 = map fst cases'
    cases''2 = map snd cases'

caseStuffTemp2 :: [(Int, AIdent)] -> AIdent -> (LLVMIdent, [(LLVMLit, LLVMIdent)], [LLVMBlock])
caseStuffTemp2 cases fallback = (labelDefault, cases''1, blockDefault : cases''2)
  where
    fnBlock :: String -> AIdent -> LLVMIdent -> LLVMBlock
    fnBlock varSuffix ident lbl =
      LLVMBlock
        lbl
        [ llvmRecord var $
          LLVMCall
            { callRef = LLVMGlobal {refName = aToLlvm ident, refType = typeFnEvaluator}
            , callArgs = [LLVMRef $ LLVMLocal (llvmIdent "record") typeFramePtr, LLVMLit $ LLVMNull typeThunkPtr]
            }
        , llvmDiscard $ LLVMRet $ Just $ LLVMRef $ LLVMLocal (llvmIdent var) typeValuePtr
        ]
      where
        var = "v-" ++ varSuffix
    fn :: Int -> (Int, AIdent) -> ((LLVMLit, LLVMIdent), LLVMBlock)
    fn i (num, destIdent) = ((LLVMInt 64 num, lbl), block)
      where
        lbl = llvmIdent $ "case-" ++ show i
        block = fnBlock (show i) destIdent lbl
    -- calculate output for the fallback case
    labelDefault = llvmIdent "case-default"
    blockDefault = fnBlock "default" fallback labelDefault
    -- calculate output for the normal cases
    cases' = zipWith fn [0 ..] cases
    cases''1 = map fst cases'
    cases''2 = map snd cases'

instance AToLlvm (AArg, Int) (LLVMValue, [(Maybe LLVMIdent, LLVMInstruction)]) where
  aToLlvm (AExt ident, argIdx) =
    argTemplate
      argIdx
      \local ->
        [ llvmRecord local $
          LLVMCall
            { callRef = LLVMGlobal (aToLlvm ident) typeFnCreator
            , callArgs = [LLVMRef $ LLVMLocal (llvmIdent "record") typeFramePtr]
            }
        ]
  aToLlvm (ARecord (ARecordIdx idx), argIdx) =
    argTemplate
      argIdx
      \local ->
        [ llvmRecord local $
          LLVMCall
            { callRef = refRecordGet
            , callArgs = [LLVMRef $ LLVMLocal (llvmIdent "record") typeFramePtr, LLVMLit $ LLVMInt 64 idx]
            }
        ]
  aToLlvm (AErased, argIdx) =
    argTemplate
      argIdx
      \local
    -- use a dummy value. shouldn't be zero because that indicates NULL. all other values are fine.
       -> [llvmRecord local $ LLVMIntToPtr (LLVMInt 64 1) typeThunkPtr]

argTemplate ::
     Int -> (String -> [(Maybe LLVMIdent, LLVMInstruction)]) -> (LLVMValue, [(Maybe LLVMIdent, LLVMInstruction)])
argTemplate argIdx instructionCreator = (LLVMRef $ LLVMLocal (llvmIdent local) typeThunkPtr, instructionCreator local)
  where
    local = "arg-" ++ show argIdx

instance AToLlvm AValue [(Maybe LLVMIdent, LLVMInstruction)] where
  aToLlvm (AValueData idx kase arity) = createData ++ createValue ++ populateValue
    where
      dataBaseSize = 24
      dataSize = dataBaseSize + 8 * arity
      createData
        -- initialize empty data base
       =
        [ llvmRecord "data_base" $ LLVMCall {callRef = refAllocData, callArgs = [LLVMLit $ LLVMInt 64 dataSize]}
        -- store idx
        , llvmRecord "data_base_idx" $
          LLVMGetElementPtr
            {elemBase = typeDataBase, elemSrc = LLVMLocal (llvmIdent "data_base") typeDataBasePtr, elemIndices = [0, 0]}
        , llvmDiscard $
          LLVMStore
            {storeSrc = LLVMLit $ LLVMInt 64 idx, storeDest = LLVMLocal (llvmIdent "data_base_idx") (LLVMPtr i64)}
        -- store case
        , llvmRecord "data_base_case" $
          LLVMGetElementPtr
            {elemBase = typeDataBase, elemSrc = LLVMLocal (llvmIdent "data_base") typeDataBasePtr, elemIndices = [0, 1]}
        , llvmDiscard $
          LLVMStore
            {storeSrc = LLVMLit $ LLVMInt 64 kase, storeDest = LLVMLocal (llvmIdent "data_base_case") (LLVMPtr i64)}
        -- store content (= current record)
        , llvmRecord "data_base_content" $
          LLVMGetElementPtr
            {elemBase = typeDataBase, elemSrc = LLVMLocal (llvmIdent "data_base") typeDataBasePtr, elemIndices = [0, 2]}
        , llvmDiscard $
          LLVMStore
            { storeSrc = LLVMRef $ LLVMLocal (llvmIdent "record") typeFramePtr
            , storeDest = LLVMLocal (llvmIdent "data_base_content") (LLVMPtr typeFramePtr)
            }
        ]
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
            { storeSrc = LLVMRef $ LLVMLocal (llvmIdent "record") typeFramePtr
            , storeDest = LLVMLocal (llvmIdent "v_fn_record") (LLVMPtr typeFramePtr)
            }
        ]
  aToLlvm (AValueLit lit) = createValue ++ populateValue
    where
      createValue = createValueTagged (litTag lit)
      populateValue
          -- cast to required form
       =
        [ llvmRecord "v_lit" $
          LLVMBitcast {bitcastFrom = LLVMLocal (llvmIdent "v") typeValuePtr, bitcastTo = tyStructPtr}
          -- get pointer to literal value
        , llvmRecord "lit" $
          LLVMGetElementPtr
            {elemBase = tyStruct, elemSrc = LLVMLocal (llvmIdent "v_lit") tyStructPtr, elemIndices = [0, 1]}
          -- store the actual value
        , llvmDiscard $ LLVMStore {storeSrc = LLVMLit litV, storeDest = LLVMLocal (llvmIdent "lit") (LLVMPtr tyValue)}
        ]
        where
          (tyStruct, tyValue) = litType lit
          tyStructPtr = LLVMPtr tyStruct
          litV = litLit lit

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
    LLVMStore {storeSrc = LLVMLit $ LLVMInt 64 tag, storeDest = LLVMLocal (llvmIdent "v_tag") (LLVMPtr i64)}
  ]

litTag :: Literal -> Int
litTag LitNat {} = 2
litTag LitWord64 {} = 3
litTag LitFloat {} = 4
litTag LitString {} = 5
litTag LitChar {} = 6
litTag _ = undefined

litType :: Literal -> (LLVMType, LLVMType)
litType LitNat {} = (typeValueCommon "lit_nat", i64)
litType LitWord64 {} = (typeValueCommon "lit_w64", i64)
litType LitFloat {} = (typeValueCommon "lit_f64", LLVMDouble)
litType LitString {} = (typeValueCommon "lit_str", i8Ptr)
litType LitChar {} = (typeValueCommon "lit_chr", i8)
litType _ = undefined

litLit :: Literal -> LLVMLit
litLit (LitNat v) = LLVMInt 64 (fromIntegral v)
litLit (LitWord64 v) = LLVMInt 64 (fromIntegral v)
litLit (LitFloat v) = LLVMDoubleV v
litLit (LitString v) = undefined -- TODO
litLit (LitChar v) = LLVMInt 8 (ord v)
litLit _ = undefined

assignNull :: String -> String -> [(Maybe LLVMIdent, LLVMInstruction)]
assignNull dest temp =
  [ llvmRecord temp $ LLVMAlloca typeValuePtr
  , llvmDiscard $
    LLVMStore
      {storeSrc = LLVMLit $ LLVMNull typeValuePtr, storeDest = LLVMLocal (llvmIdent temp) (LLVMPtr typeValuePtr)}
  , llvmRecord dest $ LLVMLoad {loadType = typeValuePtr, loadSrc = LLVMLocal (llvmIdent temp) (LLVMPtr typeValuePtr)}
  ]
