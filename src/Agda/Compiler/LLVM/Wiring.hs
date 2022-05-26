module Agda.Compiler.LLVM.Wiring where

import Agda.Compiler.Backend (IsMain(..), ModuleName(mnameToList), TCM)
import Agda.Compiler.CallCompiler (callCompiler)
import Agda.Compiler.Common (compileDir)
import Agda.Compiler.LLVM.Options (LLVMOptions)
import Agda.Compiler.LLVM.Pprint (LLVMPretty(llvmPretty))
import Agda.Compiler.LLVM.Syntax
import Agda.Utils.Pretty (prettyShow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Paths_agda2llvm
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>), takeDirectory)
import System.Posix.Internals (newFilePath)

intermediateDirName :: String
intermediateDirName = ".agda2llvm"

-- | Compute the file path to an auxiliary LLVM IR file, given its unique identifier
fileIntermediateAux :: String -> TCM FilePath
fileIntermediateAux aux = (</> intermediateDirName </> "aux" </> aux <.> "ll") <$> compileDir

-- | Compute the file path to an intermediate LLVM IR file, given a module
fileIntermediateMod :: ModuleName -> TCM FilePath
fileIntermediateMod m = do
  d <- (</> intermediateDirName </> "mod") <$> compileDir
  -- append the full module name onto the directory,
  -- where each module part becomes a directory
  -- (and the last one, implicitly, a file)
  let m' = map prettyShow $ mnameToList m
  let d' = foldl (</>) d m'
  -- add .ll extension
  return $ d' <.> "ll"

fileOutput :: IsMain -> TCM FilePath
fileOutput isMain = do
  d <- compileDir
  let ext =
        case isMain of
          IsMain -> "bin"
          NotMain -> "so"
  return $ d </> "out" <.> ext

headerPre, headerPost :: String
headerPre = ";;;; BEGIN AGDA HEADER ;;;;\n\n"

headerPost = "\n;;;; END AGDA HEADER ;;;;\n\n"

writeIntermediate :: FilePath -> LLVMModule -> IO ()
writeIntermediate p content = do
  createDirectoryIfMissing True $ takeDirectory p
  header <- getRteHeader
  let pretty = llvmPretty content
  writeFile p (headerPre ++ header ++ headerPost ++ pretty)

writeIntermediateAux :: String -> LLVMModule -> TCM FilePath
writeIntermediateAux auxName auxContent = do
  p <- fileIntermediateAux auxName
  liftIO $ writeIntermediate p auxContent
  return p

writeIntermediateModule :: ModuleName -> LLVMModule -> TCM FilePath
writeIntermediateModule modName modContent = do
  p <- fileIntermediateMod modName
  liftIO $ writeIntermediate p modContent
  return p

callLLVM :: LLVMOptions -> IsMain -> [FilePath] -> TCM ()
callLLVM opt isMain intermediates = do
  o <- fileOutput isMain
  let libs = ["gc"]
      libs' = map ("-l" ++) libs
  --let optimizeFlags = ["-O3", "-flto"]
  -- TODO: only enable these in debug mode (add flag)
  let optimizeFlags = ["-g3", "-ggdb"]
  rteFiles <- liftIO $ getRteFiles isMain
  let args = intermediates ++ rteFiles ++ ["-o", o] ++ libs' ++ optimizeFlags ++ ["-shared" | isMain == NotMain]
  -- TODO: allow path to clang to be reconfigured
  callCompiler True "clang" args Nothing

getRteFiles :: IsMain -> IO [FilePath]
getRteFiles isMain = do
  let files = ["Agda.ll", "AgdaPrim.ll", "gen" </> "AgdaPrimWrap.ll"]
  traverse getDataFileName files

getRteHeader :: IO String
getRteHeader = do
  fGeneral <- getDataFileName "header.ll"
  sGeneral <- readFile fGeneral
  fPrim <- getDataFileName $ "gen" </> "header-prim.ll"
  sPrim <- readFile fPrim
  return (sGeneral ++ sPrim)
