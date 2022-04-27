module Agda.Compiler.LLVM.Wiring where

import Agda.Compiler.Backend (ModuleName(mnameToList), TCM)
import Agda.Compiler.Common (compileDir)
import Agda.Utils.Pretty (prettyShow)
import System.FilePath ((<.>), (</>))

-- | Compute the file path to an intermediate LLVM IR file, given a module
fileIntermediate :: ModuleName -> TCM FilePath
fileIntermediate m = do
  d <- (</> ".agda2llvm") <$> compileDir
  -- append the full module name onto the directory,
  -- where each module part becomes a directory
  -- (and the last one, implicitly, a file)
  let m' = map prettyShow $ mnameToList m
  let d' = foldl (</>) d m'
  -- add .ll extension
  return $ d' <.> "ll"
