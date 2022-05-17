module Agda.Compiler.LLVM.Tables where

import Agda.Compiler.LLVM.ASyntax
import Agda.Compiler.LLVM.Syntax
import Data.Digest.CRC (CRC(crcWord), digest)
import Data.Digest.CRC64 (CRC64)
import qualified Data.String.UTF8 as BSU

computeCtorIdent :: AIdent -> (Int, Int)
computeCtorIdent ident = (value, 0)
  where
    ident' =
      case ident of
        AIdent s -> "agda2llvm." ++ s
        AIdentRaw s -> s
    crc :: CRC64 = digest $ BSU.toRep $ BSU.fromString ident'
    value = fromIntegral $ crcWord crc
