module Agda.Compiler.LLVM.Syntax where

newtype LLVMIdent =
  LLVMIdent String
  deriving (Eq, Ord)

instance Show LLVMIdent where
  show (LLVMIdent ident) = ident

data LLVMModule =
  LLVMModule
    { entries :: [LLVMEntry]
    }
  deriving (Eq)

data LLVMEntry
  = LLVMFnDecl
      { fnModifiers :: [LLVMModifier]
      , fnSign :: LLVMFnSign
      }
  | LLVMFnDefn
      { fnModifiers :: [LLVMModifier]
      , fnSign :: LLVMFnSign
      , body :: [LLVMBlock]
      }
  deriving (Eq)

data LLVMModifier =
  LLVMPrivate
  deriving (Eq)

data LLVMFnSign =
  LLVMFnSign
    { fnName :: LLVMIdent
    , fnType :: LLVMType
    , fnArgs :: [(LLVMType, LLVMIdent)]
    , fnArgsVariadic :: Bool
    }
  deriving (Eq)

data LLVMType
  = LLVMVoid
  | LLVMFn
      { fnRet :: LLVMType
      , fnParams :: [LLVMType]
      , fnVariadic :: Bool
      }
  | LLVMSizedInt
      { size :: Int
      }
  | LLVMDouble
  | LLVMPtr
      { ptrOf :: LLVMType
      }
  | LLVMArray
      { arrayElems :: Int
      , arrayType :: LLVMType
      }
  | LLVMStruct
      { structPacked :: Bool
      , structFields :: [LLVMType]
      }
  | LLVMTRef
      { typeIdent :: LLVMIdent
      }
  deriving (Eq)

data LLVMBlock =
  LLVMBlock
    { blockLabel :: LLVMIdent
    , blockInstructions :: [(Maybe LLVMIdent, LLVMInstruction)]
    }
  deriving (Eq)

data LLVMInstruction
  = LLVMAlloca
      { allocaType :: LLVMType
      }
  | LLVMBitcast
      { bitcastFrom :: LLVMRef
      , bitcastTo :: LLVMType
      }
  | LLVMCall
      { callRef :: LLVMRef
      , callArgs :: [LLVMValue]
      }
  | LLVMGetElementPtr
      { elemBase :: LLVMType
      , elemSrc :: LLVMRef
      , elemIndices :: [Int]
      }
  | LLVMIntToPtr
      { ptrValue :: LLVMLit
      , ptrType :: LLVMType
      }
  | LLVMLoad
      { loadType :: LLVMType
      , loadSrc :: LLVMRef
      }
  | LLVMRet
      { returnValue :: Maybe LLVMValue
      }
  | LLVMStore
      { storeSrc :: LLVMValue
      , storeDest :: LLVMRef
      }
  | LLVMSwitch
      { switchSubj :: LLVMValue
      , switchDefault :: LLVMIdent
      , switchBranches :: [(LLVMLit, LLVMIdent)]
      }
  | LLVMZext
      { zextFrom :: LLVMValue
      , zextTo :: LLVMType
      }
  deriving (Eq)

data LLVMValue
  = LLVMRef
      { valueRef :: LLVMRef
      }
  | LLVMLit
      { valueLit :: LLVMLit
      }
  deriving (Eq)

data LLVMRef
  = LLVMLocal
      { refName :: LLVMIdent
      , refType :: LLVMType
      }
  | LLVMGlobal
      { refName :: LLVMIdent
      , refType :: LLVMType
      }
  deriving (Eq)

data LLVMLit
  = LLVMBool
      { boolValue :: Bool
      }
  | LLVMDoubleV
      { doubleValue :: Double
      }
  | LLVMInt
      { litType :: LLVMType
      , litValue :: Int
      }
  | LLVMNull
      { litType :: LLVMType
      }
  deriving (Eq)
