module Agda.Compiler.LLVM.ToLLVM where

import Agda.Compiler.Backend
import Agda.Compiler.LLVM.Pprint (LLVMPretty(llvmPretty))
import Agda.Compiler.LLVM.RteUtil
import Agda.Compiler.LLVM.Syntax
import Agda.Compiler.LLVM.SyntaxUtil (llvmDiscard, llvmIdent, llvmRecord)
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Lens
import Agda.Utils.Pretty (prettyShow)
import Control.Monad.IO.Class (MonadIO(liftIO))

class ToLlvm a b where
  toLlvm :: a -> TCM b

instance ToLlvm Definition [LLVMEntry] where
  toLlvm def = do
    let qn = defName def
        qn' = prettyShow $ qnameName qn
    case theDef def of
      Axiom {} -> return []
      GeneralizableVar {} -> return []
      d@Function {}
        | d ^. funInline -> return []
      Function {} -> do
        let t = LLVMPtr $ LLVMArray 4 (LLVMStruct True [LLVMSizedInt 8])
        tl <- toTreeless LazyEvaluation qn
        liftIO
          do putStr "FUNCTION: "
             putStrLn $ prettyShow qn
             putStrLn $ prettyShow tl
        return
          [ LLVMFnDefn
              { fnSign = LLVMFnSign {fnName = llvmIdent qn', fnType = t, fnArgs = []}
              , body =
                  [ LLVMBlock
                      "begin"
                      [ llvmRecord "thunk_raw" $ LLVMCall {callRef = refAllocThunk, callArgs = []}
                      , llvmRecord "thunk_eval" $
                        LLVMBitcast
                          {bitcastFrom = LLVMLocal (llvmIdent "thunk_raw") typeThunkPtr, bitcastTo = typeThunkEvalPtr}
                      , llvmRecord "thunk_eval_flag" $
                        LLVMGetElementPtr
                          { elemBase = typeThunkEval
                          , elemSrc = LLVMLocal (llvmIdent "thunk_eval") typeThunkEvalPtr
                          , elemIndices = [0, 0]
                          }
                      , llvmRecord "false" $ LLVMZext {zextFrom = LLVMLit $ LLVMBool False, zextTo = LLVMSizedInt 64}
                      , llvmDiscard $
                        LLVMStore
                          { storeSrc = LLVMRef $ LLVMLocal (llvmIdent "false") (LLVMSizedInt 64)
                          , storeDest = LLVMLocal (llvmIdent "thunk_eval_flag") (LLVMPtr $ LLVMSizedInt 64)
                          }
                      , llvmDiscard $ LLVMRet $ Just $ LLVMLit $ LLVMNull t
                      ]
                  ]
              }
          ]
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
