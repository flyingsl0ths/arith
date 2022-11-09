{-# LANGUAGE RecordWildCards #-}

module Arith.Syntax.Lexer (
  Lexer,
  LexerState,
  current,
  withSource,
  scan,
  makeToken,
  singleCharToken,
  doubleCharToken,
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


type LexerState = (Lexer, AST.Token)


withSource :: String -> Lexer
withSource source =
  let current = 0
   in Lexer {..}


scan :: Lexer -> LexerState
scan lexer =
  case source lexer of
    "" -> (lexer, makeToken lexer Nothing AST.End)
    (' ' : _) -> scan $ discardUntil lexer isSpace
    ('+' : _) -> singleCharToken lexer AST.Plus
    ('-' : _) -> singleCharToken lexer AST.Minus
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
    "sin" -> func lexer "sin"
    "cos" -> func lexer "cos"
    "log" -> func lexer "log"
    "sqrt" -> func lexer "sqrt"
    "point" -> func lexer "point"
    ('.' : next : _)
      | isDigit next -> point lexer
      | otherwise -> (lexer, makeToken lexer (Just $ "Unexpected token '" ++ (next : "'")) AST.Error)
    (current : _)
      | isDigit current -> number lexer
      | otherwise -> (lexer, makeToken lexer (Just $ "Unexpected token '" ++ (current : "'")) AST.Error)


discardUntil :: Lexer -> (Char -> Bool) -> Lexer
discardUntil Lexer {..} p =
  let (_, source, n) = ASU.spanCount source p
   in Lexer {current = current + n, ..}


makeToken :: Lexer -> Maybe String -> AST.TokenType -> AST.Token
makeToken lexer lexeme type_ =
  let column = current lexer
   in AST.Token {..}


singleCharToken :: Lexer -> AST.TokenType -> LexerState
singleCharToken Lexer {..} token_type = (ctx, makeToken ctx Nothing token_type)
 where
  (_ : source) = source
  current = current + 1
  ctx = Lexer {..}


doubleCharToken :: Lexer -> AST.TokenType -> LexerState
doubleCharToken Lexer {..} token_type = (lexer, makeToken lexer Nothing token_type)
 where
  (_ : _ : source) = source
  current = current + 2
  lexer = Lexer {..}


func :: Lexer -> String -> (Lexer, AST.Token)
func Lexer {..} name =
  let n = length name
      lexer' =
        Lexer
          { source = drop n source
          , current = current + fromIntegral n
          }
   in (lexer', makeToken lexer' (Just name) AST.Function)


point :: Lexer -> LexerState
point Lexer {..} =
  let (result, source, n) = ASU.spanCount source partOfFloatingPoint
      lexer = Lexer {current = current + n, ..}
   in (lexer, makeToken lexer (Just $ '0' : result) AST.Floating)


partOfFloatingPoint :: Char -> Bool
partOfFloatingPoint c = c == '.' || isDigit c


number :: Lexer -> LexerState
number Lexer {..} =
  (lexer, makeToken lexer (Just result) token_type)
 where
  int@(_, rest, _) = ASU.spanCount source isDigit

  isFloating = Just '.' == ASU.peek rest && maybe False isDigit (ASU.peekNext rest)

  ((result, source, n), token_type) =
    if isFloating
      then (ASU.spanCount rest partOfFloatingPoint, AST.Floating)
      else (int, AST.Int)

  lexer = Lexer {current = current + n, ..}
