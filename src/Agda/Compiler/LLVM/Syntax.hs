module Agda.Compiler.LLVM.Syntax where

newtype LLVMIdent =
  LLVMIdent String

instance Show LLVMIdent where
  show (LLVMIdent ident) = ident

data LLVMModule =
  LLVMModule
    { entries :: [LLVMEntry]
    }
  deriving (Show)

data LLVMEntry
  = LLVMFnDecl
      { fnSign :: LLVMFnSign
      }
  | LLVMFnDefn
      { fnSign :: LLVMFnSign
      , body :: [LLVMBlock]
      }
  deriving (Show)

data LLVMFnSign =
  LLVMFnSign
    { fnName :: LLVMIdent
    , fnType :: LLVMType
    , fnArgs :: [(LLVMType, LLVMIdent)]
    }
  deriving (Show)

data LLVMType
  = LLVMVoid
  | LLVMFn
      { fnRet :: LLVMType
      , fnParams :: [LLVMType]
      }
  | LLVMSizedInt
      { size :: Int
      }
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
  deriving (Show)

data LLVMBlock =
  LLVMBlock
    { blockLabel :: String
    , blockInstructions :: [LLVMInstruction]
    }
  deriving (Show)

data LLVMInstruction =
  LLVMRet
    { returnValue :: Maybe LLVMValue
    }
  deriving (Show)

data LLVMValue
  = LLVMRef
      { valueRef :: LLVMRef
      }
  | LLVMLit
      { valueLit :: LLVMLit
      }
  deriving (Show)

data LLVMRef
  = LLVMLocal
      { refName :: LLVMIdent
      , refType :: LLVMType
      }
  | LLVMGlobal
      { refName :: LLVMIdent
      , refType :: LLVMType
      }
  deriving (Show)

data LLVMLit
  = LLVMInt
      { litType :: LLVMType
      , litValue :: Int
      }
  | LLVMNull
      { litType :: LLVMType
      }
  deriving (Show)
