module Agda.Compiler.LLVM.ToAbstractIntermediate where

import Agda.Compiler.Backend
import Agda.Compiler.LLVM.ASyntax
import Agda.Compiler.LLVM.Pprint (LLVMPretty(llvmPretty))
import Agda.Compiler.LLVM.RteUtil
import Agda.Compiler.LLVM.Syntax
import Agda.Compiler.LLVM.SyntaxUtil (llvmDiscard, llvmIdent, llvmRecord)
import Agda.Compiler.Treeless.NormalizeNames (normalizeNames)
import Agda.Syntax.Common (LensModality(getModality), usableModality)
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
        case tl of
          Nothing -> return Nothing
          Just tt -> do
            liftIO
              do print $ tAppView tt
                 print $ tLetView tt
                 print $ tLamView tt
            Just <$> transformFunction qn tt
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
        return Nothing
      AbstractDefn {} -> __IMPOSSIBLE__
      DataOrRecSig {} -> __IMPOSSIBLE__

---
transformFunction :: QName -> TTerm -> TCM (AIdent, [AEntry])
transformFunction qn tt = do
  let qn' = AIdent $ prettyShow qn
  return
    -- TODO
    ( qn'
    , [ AEntryThunk
          { entryIdent = AIdent "#dummy2"
          , entryThunk = AThunkDelay $ AMkValue AValueData {dataIdx = 123, dataCase = 456, dataArity = 0}
          }
      , AEntryThunk {entryIdent = qn', entryThunk = AThunkDelay $ AAppl (AIdent "#dummy2") []}
      ])
