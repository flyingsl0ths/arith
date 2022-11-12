module Arith.Syntax.Token (Token (..), TokenType (..)) where


data TokenType
  = Int
  | Floating
  | Plus
  | Minus
  | Slash
  | Modulo
  | Star
  | Hat
  | Equal
  | LeftParen
  | RightParen
  | NotEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Function
  | Error
  | End
  deriving (Eq, Show)


data Token = Token
  { type_ :: !TokenType
  , lexeme :: !(Maybe String)
  , column :: !Integer
  }
  deriving (Eq, Show)
