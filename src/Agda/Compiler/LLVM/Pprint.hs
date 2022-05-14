module Agda.Compiler.LLVM.Pprint where

import Agda.Compiler.LLVM.Syntax
import Data.List

class LLVMPretty a where
  llvmPretty :: a -> String

llvmPrettyComma :: LLVMPretty a => [a] -> String
llvmPrettyComma xs = intercalate ", " $ map llvmPretty xs

instance LLVMPretty LLVMIdent where
  llvmPretty (LLVMIdent ident) = ident

instance LLVMPretty LLVMModule where
  llvmPretty (LLVMModule entries) = intercalate "\n\n" (map llvmPretty entries)

instance LLVMPretty LLVMEntry where
  llvmPretty (LLVMFnDecl fnSign) = "declare\n" ++ llvmPretty fnSign
  llvmPretty (LLVMFnDefn fnSign body) = "define\n" ++ llvmPretty fnSign ++ "\n{\n" ++ body' ++ "}"
    where
      body' = unlines $ map llvmPretty body

instance LLVMPretty LLVMFnSign where
  llvmPretty (LLVMFnSign fnName fnType fnArgs) =
    llvmPretty fnType ++ "\n@" ++ llvmPretty fnName ++ "(" ++ fnArgs' ++ ")"
    where
      fnArgs' = intercalate ", " (map (\(t, k) -> llvmPretty t ++ " %" ++ llvmPretty k) fnArgs)

instance LLVMPretty LLVMType where
  llvmPretty LLVMVoid = "void"
  llvmPretty (LLVMFn tRet tParams variadic) =
    llvmPretty tRet ++
    "(" ++
    llvmPrettyComma tParams ++
    (if variadic
       then ", ..."
       else "") ++
    ")"
  llvmPretty (LLVMSizedInt sz) = 'i' : show sz
  llvmPretty (LLVMPtr t) = llvmPretty t ++ "*"
  llvmPretty (LLVMArray n t) = "[" ++ show n ++ " x " ++ llvmPretty t ++ "]"
  llvmPretty (LLVMStruct False fields) = "{ " ++ llvmPrettyComma fields ++ " }"
  llvmPretty (LLVMStruct True fields) = "<{ " ++ llvmPrettyComma fields ++ " }>"
  llvmPretty (LLVMTRef ident) = '%' : llvmPretty ident

instance LLVMPretty LLVMBlock where
  llvmPretty (LLVMBlock lbl instructions) = lbl ++ ":\n  " ++ instructions'
    where
      instructions' = intercalate "\n  " (map llvmPretty instructions)

instance LLVMPretty (Maybe LLVMIdent, LLVMInstruction) where
  llvmPretty (Nothing, instr) = llvmPretty instr
  llvmPretty (Just ident, instr) = '%' : llvmPretty ident ++ " = " ++ llvmPretty instr

instance LLVMPretty LLVMInstruction where
  llvmPretty (LLVMAlloca t) = "alloca " ++ llvmPretty t
  llvmPretty (LLVMBitcast from to) = "bitcast " ++ llvmPretty from ++ " to " ++ llvmPretty to
  llvmPretty (LLVMCall ref args) = "call " ++ llvmPretty ref ++ "(" ++ llvmPrettyComma args ++ ")"
  llvmPretty (LLVMGetElementPtr t ref indices) = "getelementptr " ++ llvmPretty t ++ ", " ++ llvmPretty ref ++ indices'
    where
      indices' = concatMap (\i -> ", i32 " ++ show i) indices
  llvmPretty (LLVMLoad t src) = "load " ++ llvmPretty t ++ ", " ++ llvmPretty src
  llvmPretty (LLVMRet Nothing) = "ret void"
  llvmPretty (LLVMRet (Just v)) = "ret " ++ llvmPretty v
  llvmPretty (LLVMStore src dest) = "store " ++ llvmPretty src ++ ", " ++ llvmPretty dest
  llvmPretty (LLVMSwitch subj default_ branches) =
    "switch " ++ llvmPretty subj ++ ", label %" ++ llvmPretty default_ ++ " [" ++ llvmPrettyComma branches ++ "]"
  llvmPretty (LLVMZext from to) = "zext " ++ llvmPretty from ++ " to " ++ llvmPretty to

instance LLVMPretty (LLVMLit, LLVMIdent) where
  llvmPretty (value, label) = llvmPretty value ++ ", label %" ++ llvmPretty label

instance LLVMPretty LLVMValue where
  llvmPretty (LLVMRef ref) = llvmPretty ref
  llvmPretty (LLVMLit lit) = llvmPretty lit

instance LLVMPretty LLVMRef where
  llvmPretty (LLVMLocal ident t) = llvmPretty t ++ " %" ++ llvmPretty ident
  llvmPretty (LLVMGlobal ident t) = llvmPretty t ++ " @" ++ llvmPretty ident

instance LLVMPretty LLVMLit where
  llvmPretty (LLVMBool False) = "i1 false"
  llvmPretty (LLVMBool True) = "i1 true"
  llvmPretty (LLVMInt t v) = llvmPretty t ++ " " ++ show v
  llvmPretty (LLVMNull t) = llvmPretty t ++ " null"
