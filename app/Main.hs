module Main where

import Agda.Compiler.LLVM.Compiler (llvmBackend)
import Agda.Main (runAgda)

main :: IO ()
main = runAgda [llvmBackend]
