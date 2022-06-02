module Agda.Compiler.LLVM.ToAbstractIntermediate where

import Agda.Compiler.Backend
import Agda.Compiler.LLVM.ASyntax
import Agda.Compiler.LLVM.ASyntaxUtil
import Agda.Compiler.LLVM.Pprint (LLVMPretty(llvmPretty))
import Agda.Compiler.LLVM.RteUtil
import Agda.Compiler.LLVM.Syntax
import Agda.Compiler.LLVM.SyntaxUtil (llvmDiscard, llvmIdent, llvmRecord)
import Agda.Compiler.LLVM.Tables (computeCtorIdent)
import Agda.Compiler.Treeless.NormalizeNames (normalizeNames)
import Agda.Syntax.Common (LensModality(getModality), usableModality)
import Agda.Syntax.Internal (ConHead(conName))
import Agda.Syntax.Literal
import Agda.TypeChecking.Primitive (getBuiltinName)
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Lens
import Agda.Utils.Maybe (liftMaybe)
import Agda.Utils.Pretty (prettyShow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State

type ToAM = StateT Int TCM

class ToAbstractIntermediate a b where
  toA :: a -> ToAM b

instance ToAbstractIntermediate Definition (Maybe (AIdent, [AEntry])) where
  toA def
    | defNoCompilation def || not (usableModality $ getModality def) = return Nothing
  toA def = do
    let qn = defName def
    liftIO $ putStrLn $ "DEF: " ++ prettyShow qn
    case theDef def of
      Axiom {} -> return Nothing
      GeneralizableVar {} -> return Nothing
      d@Function {}
        | d ^. funInline -> return Nothing
      Function {} -> do
        tl <-
          liftTCM
            do v <- toTreeless LazyEvaluation qn
               mapM normalizeNames v
        liftIO
          do putStr "FUNCTION: "
             putStrLn $ prettyShow qn
             -- print tl
        -- TODO: could be replaced with mapM
        case tl of
          Nothing -> return Nothing
          Just tt -> Just <$> transformFunction qn tt
      Primitive {primName = nm} -> do
        liftIO $ putStrLn $ "PRIM: " ++ nm
        Just <$> transformPrimitive qn nm
      PrimitiveSort {} -> return Nothing
      Datatype {} -> return Nothing
      Record {} -> return Nothing
      Constructor {conSrcCon = chead, conArity = nargs} -> do
        name <- toA $ conName chead
        let entries = transformCtor name nargs
        return $ Just (name, entries)
      AbstractDefn {} -> __IMPOSSIBLE__
      DataOrRecSig {} -> __IMPOSSIBLE__

instance ToAbstractIntermediate QName AIdent where
  toA = return . aIdentFromQName

---
transformFunction :: QName -> TTerm -> ToAM (AIdent, [AEntry])
transformFunction qn tt = do
  qn' <- toA qn
  (body, otherEntries) <- toA (qn', tt)
  let bodyEntry = AEntryThunk {entryIdent = qn', entryPrivate = False, entryThunk = bodyToThunkMaybeDirect body}
  return (qn', bodyEntry : otherEntries)

transformPrimitive :: QName -> String -> ToAM (AIdent, [AEntry])
transformPrimitive qn primName = do
  qn' <- toA qn
  let primIdent = AIdentRaw $ "agda.prim." ++ primName
      bodyEntry = AEntryAlias {aliasIdent = qn', aliasOf = primIdent}
  return (qn', [bodyEntry])

desugarLet :: TTerm -> TTerm
desugarLet (TLet value body) = TApp (TLam body) [value]
desugarLet _ = __IMPOSSIBLE__

instance ToAbstractIntermediate (AIdent, TTerm) (ABody, [AEntry]) where
  toA (qn, TApp subj args) = do
    (subjHolder, subjEntries) <- toArg (qn, subj)
    args' <- traverse (toArg . (qn, )) args
    let argHolders = map fst args'
        argEntries = concatMap snd args'
    return (AAppl subjHolder argHolders, subjEntries ++ argEntries)
  toA (qn, TLam body) = do
    next <- get
    modify (1 +)
    let innerName = qn <> AIdent ("--lam-" ++ show next)
    (innerBody, innerEntries) <- toA (qn, body)
    return
      ( AMkValue AValueFn {fnIdent = innerName}
      , innerEntries ++ [AEntryDirect {entryIdent = innerName, entryPushArg = True, entryBody = innerBody}])
  toA (_, TLit lit) = return (AMkValue $ AValueLit lit, [])
  toA (qn, let_@TLet {}) = toA (qn, desugarLet let_)
  toA (qn, TCase idx (CaseInfo _ ct) fallback alts) = do
    (fallbackBody, fallbackEntries) <- toA (qn, fallback)
    case ct of
      CTData {} -> do
        alts' <- traverse (toA . (qn, )) alts
        let altMatchPairs = AAData $ map fst alts'
            altEntries = concatMap snd alts'
        return (ACase (ARecordIdx idx) altMatchPairs fallbackBody, altEntries ++ fallbackEntries)
      CTNat -> do
        alts' <- traverse (toA . (qn, )) alts
        let altMatchPairs = AANat $ map fst alts'
            altEntries = concatMap snd alts'
        return (ACase (ARecordIdx idx) altMatchPairs fallbackBody, altEntries ++ fallbackEntries)
      _ -> __IMPOSSIBLE_VERBOSE__ "not implemented yet"
  toA (qn, TCoerce tt) = toA (qn, tt)
  toA (qn, TError TUnreachable) = return (AError "unreachable", [])
  toA (qn, TError (TMeta msg)) = return (AError ("meta: " ++ msg), [])
  toA (qn, tt) = do
    (arg, entries) <- toArg (qn, tt)
    return (AAppl arg [], entries)

instance ToAbstractIntermediate (AIdent, TAlt) ((AIdent, Int, ABody), [AEntry]) where
  toA (qn, TACon cn arity body) = do
    cn' <- toA cn
    -- TODO: maybe don't pass @qn@, but derived version with suffix?
    (body', bodyEntries) <- toA (qn, body)
    return ((cn', arity, body'), bodyEntries)
  toA _ = __IMPOSSIBLE_VERBOSE__ "didn't expect anything else than TACon in a data-switch"

instance ToAbstractIntermediate (AIdent, TAlt) ((Int, ABody), [AEntry]) where
  toA (qn, TALit (LitNat num) body)
    -- TODO: maybe don't pass @qn@, but derived version with suffix?
   = do
    (body', bodyEntries) <- toA (qn, body)
    return ((fromIntegral num, body'), bodyEntries)
  toA _ = __IMPOSSIBLE_VERBOSE__ "didn't expect anything else than (TALit (LitNat _) _) in a nat-literal-switch"

-- | Convert a body into a thunk.
--   In case the body is a value constructor, the value is lifted into a direct thunk, saving one indirection
bodyToThunkMaybeDirect :: ABody -> AThunk
bodyToThunkMaybeDirect (AMkValue value) = AThunkValue value
bodyToThunkMaybeDirect body = AThunkDelay body

tmpLift :: (AIdent, TTerm) -> String -> ToAM (AArg, [AEntry])
tmpLift info@(qn, tt) kind = do
  next <- get
  modify (1 +)
  -- create the body entry 'normally'
  (body, entries) <- toA info
  -- construct the lifted member and return
  let qn' = qn <> AIdent ("--" ++ kind ++ "_lift-" ++ show next)
      entry = AEntryThunk {entryIdent = qn', entryPrivate = True, entryThunk = bodyToThunkMaybeDirect body}
  return (AExt qn', entry : entries)

toArg :: (AIdent, TTerm) -> ToAM (AArg, [AEntry])
toArg (_, TVar idx) = return (ARecord $ ARecordIdx idx, [])
toArg (_, TPrim prim) = return (AExt $ AIdentRaw $ "agda.prim." ++ primIdent prim, [])
toArg (_, TDef qn) = do
  qn' <- toA qn
  return (AExt qn', [])
toArg info@(qn, TApp {}) = tmpLift info "appl"
toArg info@(qn, TLam {}) = tmpLift info "lam"
toArg info@(qn, TLit {}) = tmpLift info "lit"
toArg (_, TCon cn) = return (AExt $ AIdent $ prettyShow cn, [])
toArg (qn, let_@TLet {}) = toArg (qn, desugarLet let_)
toArg info@(qn, TCase {}) = tmpLift info "case"
toArg (_, TErased) = return (AErased, [])
toArg (qn, TCoerce tt) = toArg (qn, tt)
toArg _ = __IMPOSSIBLE_VERBOSE__ "not implemented"

primIdent :: TPrim -> String
primIdent =
  \case
    PAdd -> "add"
    PAdd64 -> "add"
    PSub -> "sub"
    PSub64 -> "sub"
    PMul -> "mul"
    PMul64 -> "mul"
    PQuot -> "quot"
    PQuot64 -> "quot"
    PRem -> "rem"
    PRem64 -> "rem"
    PGeq -> "geq"
    PLt -> "lt"
    PLt64 -> "lt64"
    PEqI -> "eqi"
    PEq64 -> "eqi"
    PEqF -> "eqf"
    PEqS -> "eqs"
    PEqC -> "eqc"
    PEqQ -> "eqq"
    PIf -> "if"
    PSeq -> "seq"
    PITo64 -> "ito64"
    P64ToI -> "64toi"

transformCtor :: AIdent -> Int -> [AEntry]
transformCtor baseName 0 =
  [ AEntryThunk
      { entryIdent = baseName
      , entryPrivate = False
      , entryThunk = AThunkDelay $ AMkValue AValueData {dataIdx = dataIdx, dataCase = dataCase, dataArity = 0}
      }
  ]
  where
    (dataIdx, dataCase) = computeCtorIdent baseName
transformCtor baseName n = go n
  where
    (dataIdx, dataCase) = computeCtorIdent baseName
    levelIdent 0 = baseName
    levelIdent m = baseName <> AIdent ('-' : show m)
    go lvl
      | lvl < 0 = __IMPOSSIBLE__
      | lvl == 0 =
        [ AEntryThunk
            { entryIdent = levelIdent lvl
            , entryPrivate = False
            , entryThunk = AThunkDelay $ AMkValue AValueFn {fnIdent = levelIdent n}
            }
        ]
      | lvl == 1 = entryFinal : go (lvl - 1)
      | otherwise = entryIntermediate : go (lvl - 1)
      where
        entryIntermediate =
          AEntryDirect
            { entryIdent = levelIdent lvl
            , entryPushArg = True
            , entryBody = AMkValue AValueFn {fnIdent = levelIdent (lvl - 1)}
            }
        entryFinal =
          AEntryDirect
            { entryIdent = levelIdent lvl
            , entryPushArg = True
            , entryBody = AMkValue AValueData {dataIdx = dataIdx, dataCase = dataCase, dataArity = n}
            }
