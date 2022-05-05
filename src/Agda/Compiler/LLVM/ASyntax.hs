module Agda.Compiler.LLVM.ASyntax where

newtype AIdent =
  AIdent String

data AEntry =
  AEntry
    { entryIdent :: AIdent
    , entryThunk :: AThunk
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

example =
  [ AEntry
      {entryIdent = AIdent "#dummy1$0", entryThunk = AThunkValue AValueData {dataIdx = 0, dataCase = 1, dataArity = 1}}
  , AEntry
      { entryIdent = AIdent "result"
      , entryThunk = AThunkDelay AAppl {applSubj = AIdent "mk", applArgs = [AIdent "#dummy2"]}
      }
  ]
