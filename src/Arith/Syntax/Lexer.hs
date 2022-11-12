{-# LANGUAGE RecordWildCards #-}

module Arith.Syntax.Lexer (
  Lexer,
  LexerState,
  current,
  withSource,
  mkLexer,
  scan,
  discardUntil,
  makeToken,
  singleCharToken,
  doubleCharToken,
  func,
  point,
  number,
) where

import qualified Arith.Syntax.Token as AST
import qualified Arith.Syntax.Utils as ASU
import Data.Char (isDigit, isSpace)


data Lexer = Lexer
  { current :: !Integer
  , source :: !String
  }
  deriving (Eq, Show)


type LexerState = (Lexer, AST.Token)


withSource :: String -> Lexer
withSource source =
  let current = 0
   in Lexer {..}


mkLexer :: Integer -> String -> Lexer
mkLexer current source = Lexer {..}


scan :: Lexer -> LexerState
scan lexer =
  case source lexer of
    "" -> (lexer, makeToken lexer Nothing AST.End)
    (' ' : _) -> scan $ discardUntil lexer isSpace
    ('+' : _) -> singleCharToken lexer AST.Plus
    ('/' : _) -> singleCharToken lexer AST.Slash
    ('%' : _) -> singleCharToken lexer AST.Modulo
    ('*' : _) -> singleCharToken lexer AST.Star
    ('^' : _) -> singleCharToken lexer AST.Hat
    ('=' : _) -> singleCharToken lexer AST.Equal
    ('(' : _) -> singleCharToken lexer AST.LeftParen
    (')' : _) -> singleCharToken lexer AST.RightParen
    ('!' : '=' : _) -> doubleCharToken lexer AST.NotEqual
    ('<' : next : _)
      | next == '=' -> doubleCharToken lexer AST.LessEqual
      | otherwise -> singleCharToken lexer AST.Less
    ('>' : next : _)
      | next == '=' -> doubleCharToken lexer AST.GreaterEqual
      | otherwise -> singleCharToken lexer AST.Greater
    ('.' : next : _)
      | isDigit next -> point lexer False
      | otherwise -> makeUnexpectedTokenError lexer {current = current lexer + 1} next
    ('-' : next : rest)
      | isDigit next -> number lexer
      | next == '.' && maybe False isDigit (ASU.peek rest) -> point lexer True
      | otherwise -> singleCharToken lexer AST.Minus
    ('s' : 'i' : 'n' : _) -> func lexer "sin"
    ('c' : 'o' : 's' : _) -> func lexer "cos"
    ('l' : 'o' : 'g' : _) -> func lexer "log"
    ('s' : 'q' : 'r' : 't' : _) -> func lexer "sqrt"
    ('p' : 'o' : 'i' : 'n' : 't' : _) -> func lexer "point"
    (current : _)
      | isDigit current -> number lexer
      | otherwise -> makeUnexpectedTokenError lexer current


discardUntil :: Lexer -> (Char -> Bool) -> Lexer
discardUntil Lexer {..} p =
  let (_, source', n) = ASU.spanCount source p
   in Lexer {current = current + n, source = source'}


makeToken :: Lexer -> Maybe String -> AST.TokenType -> AST.Token
makeToken lexer lexeme type_ =
  let column = current lexer
   in AST.Token {..}


singleCharToken :: Lexer -> AST.TokenType -> LexerState
singleCharToken Lexer {..} token_type = (lexer, makeToken lexer Nothing token_type)
 where
  (_ : source') = source
  current' = current + 1
  lexer = Lexer {current = current', source = source'}


doubleCharToken :: Lexer -> AST.TokenType -> LexerState
doubleCharToken Lexer {..} token_type = (lexer, makeToken lexer Nothing token_type)
 where
  (_ : _ : source') = source
  current' = current + 2
  lexer = Lexer {current = current', source = source'}


func :: Lexer -> String -> (Lexer, AST.Token)
func Lexer {..} name =
  let n = length name
      lexer =
        Lexer
          { source = drop n source
          , current = current + fromIntegral n
          }
   in (lexer, makeToken lexer (Just name) AST.Function)


point :: Lexer -> Bool -> LexerState
point Lexer {..} startsWithSign = (lexer, makeToken lexer (Just float') AST.Floating)
 where
  (float, source', n) = ASU.spanCount source partOfFloatingPoint
  float' = if startsWithSign then '-' : '0' : tail float else '0' : float
  lexer = Lexer {current = current + n, source = source'}


partOfFloatingPoint :: Char -> Bool
partOfFloatingPoint c = c == '-' || isDigit c || c == '.'


makeUnexpectedTokenError :: Lexer -> Char -> LexerState
makeUnexpectedTokenError lexer c =
  (lexer, makeToken lexer (Just $ "Syntax Error: Unexpected token: '" ++ (c : "'")) AST.Error)


number :: Lexer -> LexerState
number lexer =
  (lexer', makeToken lexer' (Just result') token_type)
 where
  int@(num, rest, n) = ASU.spanCount (source lexer) (\c -> c == '-' || isDigit c)

  isFloating =
    (Just '.' == ASU.peek rest)
      && maybe False isDigit (ASU.peekNext $ rest ++ "0")

  ((result, source', n'), token_type) =
    if isFloating
      then (ASU.spanCount rest partOfFloatingPoint, AST.Floating)
      else (int, AST.Int)

  result' = if isFloating then num ++ result ++ "0" else result

  lexer' =
    Lexer
      { current = if isFloating then current lexer + n + n' else current lexer + n
      , source = source'
      }
