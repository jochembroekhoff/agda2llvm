module Agda.Compiler.LLVM.Syntax where

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
    { fnName :: String
    , fnType :: LLVMType
    }
  deriving (Show)

data LLVMType
  = LLVMSizedInt
      { size :: Int
      }
  | LLVMPtr
      { ptrOf :: LLVMType
      }
  deriving (Show)

data LLVMBlock =
  LLVMBlock
    { blockLabel :: String
    , blockInstructions :: [LLVMInstruction]
    }
  deriving (Show)

data LLVMInstruction
  = LLVMRetVoid
  | LLVMRetValue
      { returnType :: LLVMType
      , returnValue :: LLVMValue
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
      { refName :: String
      , refType :: LLVMType
      }
  | LLVMGlobal
      { refName :: String
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
