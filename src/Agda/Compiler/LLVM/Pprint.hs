module Agda.Compiler.LLVM.Pprint where

import Agda.Compiler.LLVM.Syntax
import Data.List

class LLVMPretty a where
  llvmPretty :: a -> String

instance LLVMPretty LLVMIdent where
  llvmPretty (LLVMIdent ident) = ident

instance LLVMPretty LLVMModule where
  llvmPretty (LLVMModule entries) = intercalate "\n\n" (map llvmPretty entries)

instance LLVMPretty LLVMEntry where
  llvmPretty (LLVMFnDecl fnSign) = "declare " ++ llvmPretty fnSign
  llvmPretty (LLVMFnDefn fnSign body) = "define " ++ llvmPretty fnSign ++ " {\n" ++ body' ++ "}"
    where
      body' = unlines $ map llvmPretty body

instance LLVMPretty LLVMFnSign where
  llvmPretty (LLVMFnSign fnName fnType fnArgs) = llvmPretty fnType ++ " @" ++ llvmPretty fnName ++ "(" ++ fnArgs' ++ ")"
    where
      fnArgs' = intercalate ", " (map (\(k, t) -> llvmPretty t ++ " %" ++ llvmPretty k) fnArgs)

instance LLVMPretty LLVMType where
  llvmPretty LLVMVoid = "void"
  llvmPretty (LLVMFn tRet tParams) = llvmPretty tRet ++ " (" ++ intercalate ", " (map llvmPretty tParams)
  llvmPretty (LLVMSizedInt sz) = 'i' : show sz
  llvmPretty (LLVMPtr t) = llvmPretty t ++ "*"
  llvmPretty (LLVMArray n t) = "[" ++ show n ++ " x " ++ llvmPretty t ++ "]"
  llvmPretty (LLVMStruct False fields) = "{ " ++ intercalate ", " (map llvmPretty fields) ++ " }"
  llvmPretty (LLVMStruct True fields) = "<{ " ++ intercalate ", " (map llvmPretty fields) ++ " }>"

instance LLVMPretty LLVMBlock where
  llvmPretty (LLVMBlock lbl instructions) = lbl ++ ":\n  " ++ instructions'
    where
      instructions' = intercalate "\n  " (map llvmPretty instructions)

instance LLVMPretty LLVMInstruction where
  llvmPretty (LLVMRet Nothing) = "ret void"
  llvmPretty (LLVMRet (Just v)) = "ret " ++ llvmPretty v

instance LLVMPretty LLVMValue where
  llvmPretty (LLVMRef ref) = llvmPretty ref
  llvmPretty (LLVMLit lit) = llvmPretty lit

instance LLVMPretty LLVMRef where
  llvmPretty (LLVMLocal ident t) = llvmPretty t ++ " %" ++ llvmPretty ident
  llvmPretty (LLVMGlobal ident t) = llvmPretty t ++ " @" ++ llvmPretty ident

instance LLVMPretty LLVMLit where
  llvmPretty (LLVMInt t v) = llvmPretty t ++ " " ++ show v
  llvmPretty (LLVMNull t) = llvmPretty t ++ " null"
