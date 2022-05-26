module Agda.Compiler.LLVM.ASyntaxUtil where

import Agda.Compiler.Backend
import Agda.Compiler.LLVM.ASyntax
import Agda.Utils.Pretty (prettyShow)

aIdentFromQName :: QName -> AIdent
aIdentFromQName = AIdent . prettyShow
