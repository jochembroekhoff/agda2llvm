{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse_" #-}
module Agda.Compiler.LLVM.Compiler where

import Agda.Compiler.Backend
import Agda.Compiler.Common (compileDir)
import Agda.Compiler.LLVM.Options (LLVMOptions, defaultLLVMOptions)
import Agda.Compiler.LLVM.Pprint (LLVMPretty(llvmPretty))
import Agda.Compiler.LLVM.RteUtil
import Agda.Compiler.LLVM.Syntax
import Agda.Compiler.LLVM.SyntaxUtil (llvmDiscard, llvmIdent, llvmRecord)
import Agda.Compiler.LLVM.Wiring (callLLVM, fileIntermediate, writeIntermediate)
import Agda.Interaction.Options (OptDescr)
import Agda.Utils.Impossible (__IMPOSSIBLE__)
import Agda.Utils.Lens
import Agda.Utils.Pretty (prettyShow)
import Agda.Utils.Tuple (mapFstM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

llvmBackendName = "LLVM"

llvmBackend :: Backend
llvmBackend = Backend llvmBackend'

llvmBackend' :: Backend' LLVMOptions LLVMOptions LLVMEnv LLVMModule [LLVMEntry]
llvmBackend' =
  Backend'
    { backendName = llvmBackendName
    , backendVersion = Nothing
    , options = defaultLLVMOptions
    , commandLineFlags = llvmCommandLineFlags
    , isEnabled = const True
    , preCompile = llvmPreCompile
    , postCompile = llvmPostCompile
    , preModule = \_ _ _ _ -> return $ Recompile LLVMEnv {}
    , postModule = llvmPostModule
    , compileDef = llvmCompileDef
    , scopeCheckingSuffices = False
    , mayEraseType = const $ return True
    }

--- CLI flags ---
llvmCommandLineFlags :: [OptDescr (Flag LLVMOptions)]
llvmCommandLineFlags = []

--- Envs ---
data LLVMEnv =
  LLVMEnv
    {
    }

--- Compilation start & finish handlers ---
llvmPreCompile :: LLVMOptions -> TCM LLVMOptions
llvmPreCompile = return

llvmPostCompile :: LLVMOptions -> IsMain -> Map ModuleName LLVMModule -> TCM ()
llvmPostCompile opts isMain modules
   -- TODO: write meta.ll with string and case tables
 = do
  modules' <- traverse (uncurry writeIntermediate) $ Map.toList modules
  callLLVM opts isMain modules'

--- Module & defs compilation ---
llvmPostModule :: LLVMOptions -> LLVMEnv -> IsMain -> ModuleName -> [[LLVMEntry]] -> TCM LLVMModule
llvmPostModule _ _ main m defs = do
  d <- compileDir
  let m' = mnameToList m
  interm <- fileIntermediate m
  liftIO
    do putStrLn $ "Module: " ++ prettyShow m'
       putStrLn $ "IsMain: " ++ show (main == IsMain)
       putStrLn $ "CompileDir: " ++ show d
       putStrLn $ "IntermediateFile: " ++ show interm
  -- TODO: add "define @main" if isMain==IsMain
  -- TODO: add header entries (instead of string concat)
  return $ LLVMModule {entries = concat defs}

llvmCompileDef :: LLVMOptions -> LLVMEnv -> IsMain -> Definition -> TCM [LLVMEntry]
llvmCompileDef _ _ _ def = do
  toLlvm def

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
                          {bitcastFrom = LLVMLocal (LLVMIdent "thunk_raw") typeThunkPtr, bitcastTo = typeThunkEvalPtr}
                      , llvmRecord "thunk_eval_flag" $
                        LLVMGetElementPtr
                          { elemBase = typeThunkEval
                          , elemSrc = LLVMLocal (LLVMIdent "thunk_eval") typeThunkEvalPtr
                          , elemIndices = [0, 0]
                          }
                      , llvmRecord "false" $ LLVMZext {zextFrom = LLVMLit $ LLVMBool False, zextTo = LLVMSizedInt 64}
                      , llvmDiscard $
                        LLVMStore
                          { storeSrc = LLVMRef $ LLVMLocal (LLVMIdent "false") (LLVMSizedInt 64)
                          , storeDest = LLVMLocal (LLVMIdent "thunk_eval_flag") (LLVMPtr $ LLVMSizedInt 64)
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
