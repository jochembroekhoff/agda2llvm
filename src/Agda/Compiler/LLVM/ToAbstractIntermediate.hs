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

class ToAbstractIntermediate a b where
  toA :: a -> TCM b

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
        liftIO
          do putStr "CONSTRUCTOR: "
             putStrLn $ prettyShow qn
             putStrLn $ prettyShow chead
             print nargs
        name <- toA $ conName chead
        let entries = transformCtor name (123, 456, nargs)
        return $ Just (name, entries)
      AbstractDefn {} -> __IMPOSSIBLE__
      DataOrRecSig {} -> __IMPOSSIBLE__

instance ToAbstractIntermediate QName AIdent where
  toA qn = return $ AIdent $ prettyShow qn

---
transformFunction :: QName -> TTerm -> TCM (AIdent, [AEntry])
transformFunction qn tt = do
  let (appl, args) = tAppView tt
  qn' <- toA qn
  applEntries <- transformIdentifiedAppl qn' (appl, args)
  return (qn', applEntries)

transformIdentifiedAppl :: AIdent -> (TTerm, [TTerm]) -> TCM [AEntry]
transformIdentifiedAppl qn' (appl, args) = do
  (applHolder, applEntries) <- transformAnonymousTermLifted appl
  args' <- traverse transformAnonymousTermLifted args
  let argHolders = map fst args'
      argEntries = concatMap snd args'
  let bodyEntry = AEntryThunk {entryIdent = qn', entryThunk = AThunkDelay $ AAppl applHolder argHolders}
  return (applEntries ++ argEntries ++ [bodyEntry])

transformAnonymousTermLifted :: TTerm -> TCM (AArg, [AEntry])
transformAnonymousTermLifted (TVar idx) = return (ARecord idx, [])
transformAnonymousTermLifted (TPrim _) = __IMPOSSIBLE_VERBOSE__ "not implemented"
transformAnonymousTermLifted (TDef qn) = do
  qn' <- toA qn
  return (AExt qn', [])
transformAnonymousTermLifted (TApp appl args) = do
  let name' = AIdent "app-0"
  applEntries <- transformIdentifiedAppl name' (appl, args)
  return (AExt name', applEntries)
transformAnonymousTermLifted (TLam inner) = do
  let name' = AIdent "lam-0"
      name'_inner = name' <> AIdent "-inner"
  (innerIdent, innerEntries) <- transformAnonymousTermLifted inner
  return
    ( AExt name'
    , innerEntries ++
      [ AEntryThunk {entryIdent = name', entryThunk = AThunkValue AValueFn {fnIdent = name'_inner}}
      , AEntryDirect {entryIdent = name'_inner, entryPushArg = True, entryBody = AAppl innerIdent []}
      ])
transformAnonymousTermLifted (TCon cn) = return (AExt $ AIdent $ prettyShow cn, [])
transformAnonymousTermLifted _ = __IMPOSSIBLE_VERBOSE__ "not implemented"

transformCtor :: AIdent -> (Int, Int, Int) -> [AEntry]
transformCtor baseName (dataIdx, dataCase, 0) =
  [ AEntryThunk
      { entryIdent = baseName
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
            {entryIdent = levelIdent lvl, entryThunk = AThunkDelay $ AMkValue AValueFn {fnIdent = levelIdent n}}
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
