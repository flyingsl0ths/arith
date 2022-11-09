module Arith.VM.OpCodes where


data Value
  = SignedNumber !Integer
  | FloatingPointNumber !Double


data OpCode
  = Add
  | Sub
  | Div
  | Mod
  | Mul
  | Pow
  | Eql
  | NotEql
  | Grtr
  | GrtrEql
  | Lss
  | LssEql
  | Constant !Value
  | Call !(Value -> Value)
