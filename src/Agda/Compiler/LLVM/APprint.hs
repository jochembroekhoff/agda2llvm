module Agda.Compiler.LLVM.APprint where

import Agda.Compiler.LLVM.ASyntax
import Data.List

mapLines :: (String -> String) -> String -> String
mapLines fn = unlines . map fn . lines

indent :: String -> String
indent = mapLines ("    " ++)

class APretty a where
  aPretty :: a -> String

instance APretty AIdent where
  aPretty (AIdent ident) = ident

instance APretty AEntry where
  aPretty (AEntry ident thunk) = aPretty ident ++ "() =\n" ++ indent (aPretty thunk)

instance APretty AThunk where
  aPretty (AThunkDelay body) = "THUNK.delay{\n" ++ indent (aPretty body) ++ "}"
  aPretty (AThunkValue value) = "THUNK.value{" ++ aPretty value ++ "}"

instance APretty ABody where
  aPretty (AMkValue v) = "ret " ++ aPretty v
  aPretty (AAppl subj args) =
    unlines
      ["appl = " ++ aPretty subj ++ "()", "-- TODO: " ++ show (length args) ++ " arg(s)", "ret APPL(appl, ARGS...)"]

instance APretty AValue where
  aPretty (AValueData idx kase arity) =
    "VALUE.data{" ++ show idx ++ "," ++ show kase ++ ";todo_args=" ++ show arity ++ "}"
  aPretty (AValueFn fnIdent) = "VALUE.fn{" ++ aPretty fnIdent ++ "}"
