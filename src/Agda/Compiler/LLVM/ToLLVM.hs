module Agda.Compiler.LLVM.ToLLVM where

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

class ToLlvm a b where
  toLlvm :: a -> TCM b

instance ToLlvm Definition [AEntry] where
  toLlvm def
    | defNoCompilation def || not (usableModality $ getModality def) = return []
  toLlvm def = do
    let qn = defName def
    case theDef def of
      Axiom {} -> return []
      GeneralizableVar {} -> return []
      d@Function {}
        | d ^. funInline -> return []
      Function {} -> do
        tl <-
          do v <- toTreeless LazyEvaluation qn
             mapM normalizeNames v
        liftIO
          do putStr "FUNCTION: "
             putStrLn $ prettyShow qn
             print tl
        case tl of
          Nothing -> return []
          Just tt -> do
            liftIO
              do print $ tAppView tt
                 print $ tLetView tt
                 print $ tLamView tt
            transformFunction qn tt
      Primitive {} -> return []
      PrimitiveSort {} -> return []
      Datatype {} -> return []
      Record {} -> return []
      Constructor {conSrcCon = chead, conArity = nargs} -> do
        liftIO
          do putStr "CONSTRUCTOR: "
             putStrLn $ prettyShow qn
             putStrLn $ prettyShow chead
             print nargs
        return []
      AbstractDefn {} -> __IMPOSSIBLE__
      DataOrRecSig {} -> __IMPOSSIBLE__

---
transformFunction :: QName -> TTerm -> TCM [AEntry]
transformFunction qn tt = do
  let t = LLVMPtr $ LLVMArray 4 (LLVMStruct True [LLVMSizedInt 8])
      qn' = prettyShow qn
  return
    -- TODO
    [ AEntryThunk
        { entryIdent = AIdent "#dummy2"
        , entryThunk = AThunkDelay $ AMkValue AValueData {dataIdx = 0, dataCase = 1, dataArity = 0}
        }
    , AEntryThunk {entryIdent = AIdent qn', entryThunk = AThunkDelay $ AAppl (AIdent "#dummy2") []}
    ]
