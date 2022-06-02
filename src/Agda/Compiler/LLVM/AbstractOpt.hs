module Agda.Compiler.LLVM.AbstractOpt where

import Agda.Compiler.LLVM.ASyntax
import Agda.Compiler.LLVM.AbstractToLLVM (AToLlvm)

class AbstractOptimize a where
  abstractOptimize :: a -> a

instance AbstractOptimize AEntry where
  abstractOptimize eT@AEntryThunk {entryThunk = thunk} = eT {entryThunk = abstractOptimize thunk}
  abstractOptimize x = x

instance AbstractOptimize AThunk where
  abstractOptimize (AThunkDelay (AMkValue v)) = AThunkValue v
  abstractOptimize x = x
