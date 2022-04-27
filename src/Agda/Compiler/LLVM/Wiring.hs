module Agda.Compiler.LLVM.Wiring where

import Agda.Compiler.Backend (IsMain(..), ModuleName(mnameToList), TCM)
import Agda.Compiler.CallCompiler (callCompiler)
import Agda.Compiler.Common (compileDir)
import Agda.Compiler.LLVM.Options (LLVMOptions)
import Agda.Utils.Pretty (prettyShow)
import System.FilePath ((<.>), (</>))
import System.Posix.Internals (newFilePath)

intermediateDirName :: String
intermediateDirName = ".agda2llvm"

-- | Compute the file path to an intermediate LLVM IR file, given a module
fileIntermediate :: ModuleName -> TCM FilePath
fileIntermediate m = do
  d <- (</> intermediateDirName) <$> compileDir
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

callLLVM :: LLVMOptions -> IsMain -> [FilePath] -> TCM ()
callLLVM opt isMain intermediates = do
  o <- fileOutput isMain
  let args = intermediates ++ ["-o", o] ++ ["-shared" | isMain == NotMain]
  -- TODO: allow path to clang to be reconfigured
  callCompiler True "clang" args Nothing
