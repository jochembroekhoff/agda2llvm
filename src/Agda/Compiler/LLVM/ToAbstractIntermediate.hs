module Agda.Compiler.LLVM.ToAbstractIntermediate where

import Agda.Compiler.Backend
import Agda.Compiler.LLVM.ASyntax
import Agda.Compiler.LLVM.Pprint (LLVMPretty(llvmPretty))
import Agda.Compiler.LLVM.RteUtil
import Agda.Compiler.LLVM.Syntax
import Agda.Compiler.LLVM.SyntaxUtil (llvmDiscard, llvmIdent, llvmRecord)
import Agda.Compiler.Treeless.NormalizeNames (normalizeNames)
import Agda.Syntax.Common (LensModality(getModality), usableModality)
import Agda.Syntax.Internal (ConHead(conName))
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
             print tl
        -- TODO: could be replaced with mapM
        case tl of
          Nothing -> return Nothing
          Just tt -> Just <$> transformFunction qn tt
      Primitive {} -> return Nothing
      PrimitiveSort {} -> return Nothing
      Datatype {} -> return Nothing
      Record {} -> return Nothing
      Constructor {conSrcCon = chead, conArity = nargs} -> do
        name <- toA $ conName chead
        let entries = transformCtor name (123, 456, nargs)
        return $ Just (name, entries)
      AbstractDefn {} -> __IMPOSSIBLE__
      DataOrRecSig {} -> __IMPOSSIBLE__

instance ToAbstractIntermediate QName AIdent where
  toA qn = return $ AIdent $ prettyShow qn

---
transformFunction :: QName -> TTerm -> ToAM (AIdent, [AEntry])
transformFunction qn tt = do
  let (appl, args) = tAppView tt
  qn' <- toA qn
  applEntries <- transformIdentifiedAppl qn' (appl, args) False
  return (qn', applEntries)

transformIdentifiedAppl :: AIdent -> (TTerm, [TTerm]) -> Bool -> ToAM [AEntry]
transformIdentifiedAppl qn' (appl, args) private = do
  (applHolder, applEntries) <- toA (qn', appl)
  args' <- traverse (toA . (qn', )) args
  let argHolders = map fst args'
      argEntries = concatMap snd args'
  let bodyEntry =
        AEntryThunk {entryIdent = qn', entryPrivate = private, entryThunk = AThunkDelay $ AAppl applHolder argHolders}
  return (applEntries ++ argEntries ++ [bodyEntry])

instance ToAbstractIntermediate (AIdent, TTerm) (AArg, [AEntry]) where
  toA (_, TVar idx) = return (ARecord idx, [])
  toA (_, TPrim _) = __IMPOSSIBLE_VERBOSE__ "not implemented"
  toA (_, TDef qn) = do
    qn' <- toA qn
    return (AExt qn', [])
  toA (qn, TApp appl args) = do
    next <- get
    modify (1 +)
    let name' = qn <> AIdent ("--app-" ++ show next)
    applEntries <- transformIdentifiedAppl name' (appl, args) True
    return (AExt name', applEntries)
  toA (qn, TLam inner) = do
    next <- get
    modify (1 +)
    let name' = qn <> AIdent ("--lam-" ++ show next)
        name'_inner = name' <> AIdent "-inner"
    (innerIdent, innerEntries) <- toA (qn, inner)
    return
      ( AExt name'
      , innerEntries ++
        [ AEntryThunk
            {entryIdent = name', entryPrivate = True, entryThunk = AThunkValue AValueFn {fnIdent = name'_inner}}
        , AEntryDirect {entryIdent = name'_inner, entryPushArg = True, entryBody = AAppl innerIdent []}
        ])
  toA (_, TCon cn) = return (AExt $ AIdent $ prettyShow cn, [])
  toA (qn, TCoerce tt) = toA (qn, tt)
  toA _ = __IMPOSSIBLE_VERBOSE__ "not implemented"

transformCtor :: AIdent -> (Int, Int, Int) -> [AEntry]
transformCtor baseName (dataIdx, dataCase, 0) =
  [ AEntryThunk
      { entryIdent = baseName
      , entryPrivate = False
      , entryThunk = AThunkDelay $ AMkValue AValueData {dataIdx = dataIdx, dataCase = dataCase, dataArity = 0}
      }
  ]
transformCtor baseName (dataIdx, dataCase, n) = go n
  where
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
