module Agda.Compiler.LLVM.ASyntax where

newtype AIdent =
  AIdent String

data AEntry
  = AEntryThunk
      { entryIdent :: AIdent
      , entryThunk :: AThunk
      }
  | AEntryDirect
      { entryIdent :: AIdent
      , entryBody :: ABody
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
      { applSubj :: AIdent
      , applArgs :: [AIdent]
      }

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
