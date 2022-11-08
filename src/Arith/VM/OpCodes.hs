module Arith.VM.OpCodes where


data Value
  = SignedNumber !Integer
  | FloatingPointNumber !Double
  deriving (Show)


data OpCode
  = Constant !Value
  | Add
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
  deriving (Show)
