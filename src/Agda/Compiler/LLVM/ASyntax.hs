module Agda.Compiler.LLVM.ASyntax where

data AIdent
  = AIdent String
  | AIdentRaw String

data AEntry
  = AEntryThunk
      { entryIdent :: AIdent
      , entryPrivate :: Bool
      , entryThunk :: AThunk
      }
  | AEntryDirect
      { entryIdent :: AIdent
      , entryPushArg :: Bool
      , entryBody :: ABody
      }
  | AEntryMain
      { mainReference :: AIdent
      }

data AThunk
  = AThunkDelay
      { thunkDelayBody :: ABody
      }
  | AThunkValue
      { thunkValueValue :: AValue
      }

data ABody
  = AMkValue
      { value :: AValue
      }
  | AAppl
      { applSubj :: AArg
      , applArgs :: [AArg]
      }
  | ACase
      { caseSubj :: ARecordIdx
      , caseAlts :: [(AIdent, ABody)]
      , caseFallback :: ABody
      }
  | AError
      { errorText :: String
      }

newtype ARecordIdx =
  ARecordIdx Int

data AArg
  = AExt
      { extIdent :: AIdent
      }
  | ARecord
      { recordIdx :: ARecordIdx
      }
  | AErased

data AValue
  = AValueData
      { dataIdx :: Int
      , dataCase :: Int
      , dataArity :: Int
      }
  | AValueFn
      { fnIdent :: AIdent
      }

---
instance Semigroup AIdent where
  AIdent l <> AIdent r = AIdent (l <> r)
  AIdentRaw l <> AIdentRaw r = AIdentRaw (l <> r)
  _ <> _ = error "illegal associative operation"
