{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use traverse_" #-}
module Agda.Compiler.LLVM.Compiler where

import Agda.Compiler.Backend
import Agda.Compiler.Common (compileDir)
import Agda.Compiler.LLVM.ASyntax
import Agda.Compiler.LLVM.AbstractToLLVM (AToLlvm(aToLlvm))
import Agda.Compiler.LLVM.Options (LLVMOptions, defaultLLVMOptions)
import Agda.Compiler.LLVM.Pprint (LLVMPretty(llvmPretty))
import Agda.Compiler.LLVM.RteUtil
import Agda.Compiler.LLVM.Syntax
import Agda.Compiler.LLVM.ToAbstractIntermediate (ToAbstractIntermediate(toA))
import Agda.Compiler.LLVM.Wiring (callLLVM, fileIntermediate, writeIntermediate)
import Agda.Interaction.Options (OptDescr)
import Agda.Utils.Pretty (prettyShow)
import Agda.Utils.Tuple (mapFstM)
import Control.Monad.IO.Class (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

llvmBackendName = "LLVM"

llvmBackend :: Backend
llvmBackend = Backend llvmBackend'

llvmBackend' :: Backend' LLVMOptions LLVMOptions LLVMEnv LLVMModule [AEntry]
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
llvmPostModule :: LLVMOptions -> LLVMEnv -> IsMain -> ModuleName -> [[AEntry]] -> TCM LLVMModule
llvmPostModule _ _ main m defs = do
  d <- compileDir
  let m' = mnameToList m
  interm <- fileIntermediate m
  liftIO
    do putStrLn $ "Module: " ++ prettyShow m'
       putStrLn $ "IsMain: " ++ show (main == IsMain)
       putStrLn $ "CompileDir: " ++ show d
       putStrLn $ "IntermediateFile: " ++ show interm
  -- TODO: check that there is a definition @main iff main==IsMain
  -- TODO: add header entries (instead of string concat)
  let defs' = concatMap aToLlvm $ concat defs
  return $ LLVMModule {entries = defs'}

llvmCompileDef :: LLVMOptions -> LLVMEnv -> IsMain -> Definition -> TCM [AEntry]
llvmCompileDef _ _ isMain def = do
  res <- toA def
  case res of
    Nothing -> return []
    Just (name, entries) -> return (entries ++ [AEntryMain name | thisDefIsTheMainOne isMain def])

thisDefIsTheMainOne :: IsMain -> Definition -> Bool
thisDefIsTheMainOne NotMain _ = False
thisDefIsTheMainOne IsMain def = prettyShow (qnameName $ defName def) == "main"
