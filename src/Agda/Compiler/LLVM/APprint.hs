module Agda.Compiler.LLVM.APprint where

import Agda.Compiler.LLVM.ASyntax
import Agda.Utils.Pretty (prettyShow)
import Agda.Utils.String (quote)
import Data.List

mapLines :: (String -> String) -> String -> String
mapLines fn = unlines . map fn . lines

indent :: String -> String
indent = mapLines ("    " ++)

class APretty a where
  aPretty :: a -> String

instance APretty AIdent where
  aPretty (AIdent ident) = ident
  aPretty (AIdentRaw identRaw) = identRaw

instance APretty AEntry where
  aPretty (AEntryThunk ident _ thunk) = aPretty ident ++ "() =\n" ++ indent (aPretty thunk)
  aPretty (AEntryDirect ident False body) = aPretty ident ++ "() =\n" ++ indent (aPretty body)
  aPretty (AEntryDirect ident True body) = aPretty ident ++ "(p) =\n" ++ indent (aPretty body)
  aPretty (AEntryMain mainIdent) = "main() = <runtime-main>(" ++ aPretty mainIdent ++ ")"

instance APretty AThunk where
  aPretty (AThunkDelay body) = "THUNK.delay{\n" ++ indent (aPretty body) ++ "}"
  aPretty (AThunkValue value) = "THUNK.value{" ++ aPretty value ++ "}"

instance APretty ABody where
  aPretty (AMkValue v) = "ret " ++ aPretty v
  aPretty (AAppl subj args) =
    unlines
      ["appl = " ++ aPretty subj ++ "()", "-- TODO: " ++ show (length args) ++ " arg(s)", "ret APPL(appl, ARGS...)"]
  aPretty (ACase subj alts fallback) =
    unlines ["case_scrutinee = " ++ aPretty subj, "-- TODO: alternative(s)", "ret CASE(subj, ALTS...)"]
  aPretty (AError text) = "ERROR " ++ quote text

instance APretty ARecordIdx where
  aPretty (ARecordIdx idx) = "R#" ++ show idx

instance APretty AArg where
  aPretty (AExt extIdent) = aPretty extIdent ++ "()"
  aPretty (ARecord idx) = "<record " ++ aPretty idx ++ ">"
  aPretty AErased = "<erased>"

instance APretty AValue where
  aPretty (AValueData idx kase arity) =
    "VALUE.data{" ++ show idx ++ "," ++ show kase ++ ";todo_args=" ++ show arity ++ "}"
  aPretty (AValueFn fnIdent) = "VALUE.fn{" ++ aPretty fnIdent ++ "}"
  aPretty (AValueLit lit) = "VALUE.lit{" ++ prettyShow lit ++ "}"
