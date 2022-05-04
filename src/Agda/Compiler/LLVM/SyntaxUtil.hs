module Agda.Compiler.LLVM.SyntaxUtil where

import Agda.Compiler.LLVM.Syntax (LLVMIdent(LLVMIdent), LLVMInstruction)
import Data.Char (ord)

-- | Safely create an identifier
llvmIdent :: String -> LLVMIdent
llvmIdent = LLVMIdent . sanitize . ("agda2llvm." ++)
  where
    valid c = c `elem` "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$._-"
    sanitize str
      | any (not . valid) str = "\"" ++ sanitize' str ++ "\""
      | otherwise = str
    sanitize' "" = ""
    sanitize' (c:cs)
      | valid c = c : cs'
      | otherwise = '\\' : toHex (ord c) ++ cs'
      where
        cs' = sanitize' cs

llvmRecord :: String -> LLVMInstruction -> (Maybe LLVMIdent, LLVMInstruction)
llvmRecord ident = (Just $ llvmIdent ident, )

llvmDiscard :: LLVMInstruction -> (Maybe LLVMIdent, LLVMInstruction)
llvmDiscard = (Nothing, )

-- | Convert a number to its lowercase hexadecimal representation.
--   (from @agda2scheme@)
toHex :: Int -> String
toHex 0 = ""
toHex i = toHex (i `div` 16) ++ [fourBitsToChar (i `mod` 16)]
  where
    fourBitsToChar i = "0123456789abcdef" !! i
    {-# INLINE fourBitsToChar #-}
