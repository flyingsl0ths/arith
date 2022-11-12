import qualified Arith.Syntax.Lexer as ASL
import qualified Arith.Syntax.Token as AST
import qualified Arith.Syntax.Utils as ASU
import Control.Monad (mapM_)
import Data.Char (isSpace)
import Test.Hspec (describe, hspec, it, shouldBe)


main :: IO ()
main = hspec $ do
  describe "Arith.Syntax.Lexer.discardUntil" $ do
    it "discards until there are no more whitespace characters" $ do
      ASL.discardUntil (ASL.withSource "   1 + 1") isSpace `shouldBe` ASL.mkLexer 3 "1 + 1"

  describe "Arith.Syntax.Lexer.makeToken" $ do
    it "returns a token representing an integer when the lexer finds an integer" $ do
      ASL.makeToken (ASL.mkLexer 1 " + 1") (Just "1") AST.Int
        `shouldBe` AST.Token
          { AST.type_ = AST.Int
          , AST.lexeme = Just "1"
          , AST.column = 1
          }

  describe "Arith.Syntax.Lexer.singleCharToken" $ do
    it "updates the lexer accordingly and returns a token represented by a single character" $ do
      ASL.singleCharToken (ASL.mkLexer 2 "= 1") AST.Equal
        `shouldBe` ( ASL.mkLexer 3 " 1"
                   , AST.Token
                      { AST.type_ = AST.Equal
                      , AST.lexeme = Nothing
                      , AST.column = 3
                      }
                   )

  describe "Arith.Syntax.Lexer.doubleCharToken" $ do
    it "updates the lexer accordingly and returns a token represented by two characters" $ do
      ASL.doubleCharToken (ASL.mkLexer 2 ">= 1") AST.GreaterEqual
        `shouldBe` ( ASL.mkLexer 4 " 1"
                   , AST.Token
                      { AST.type_ = AST.GreaterEqual
                      , AST.lexeme = Nothing
                      , AST.column = 4
                      }
                   )

  describe "Arith.Syntax.Lexer.func" $ do
    it "updates the lexer accordingly and returns a token representing a built-in function" $ do
      ASL.func (ASL.mkLexer 2 "sin(1.0)") "sin"
        `shouldBe` ( ASL.mkLexer 5 "(1.0)"
                   , AST.Token
                      { AST.type_ = AST.Function
                      , AST.lexeme = Just "sin"
                      , AST.column = 5
                      }
                   )

  describe "Arith.Syntax.Lexer.point" $ do
    it
      ( "updates the lexer accordingly and returns a token"
          ++ "representing an abbreviated signed floating point number"
      )
      $ do
        ASL.point (ASL.withSource "-.10") True
          `shouldBe` ( ASL.mkLexer 4 ""
                     , AST.Token
                        { AST.type_ = AST.Floating
                        , AST.lexeme = Just "-0.10"
                        , AST.column = 4
                        }
                     )

    it
      ( "updates the lexer accordingly and returns a token"
          ++ "representing an abbreviated floating point number"
      )
      $ do
        ASL.point (ASL.withSource ".10") False
          `shouldBe` ( ASL.mkLexer 3 ""
                     , AST.Token
                        { AST.type_ = AST.Floating
                        , AST.lexeme = Just "0.10"
                        , AST.column = 3
                        }
                     )

  describe "Arith.Syntax.Lexer.number" $ do
    it "updates the lexer accordingly and returns a token representing an positive number" $ do
      ASL.number (ASL.withSource "10")
        `shouldBe` ( ASL.mkLexer 2 ""
                   , AST.Token
                      { AST.type_ = AST.Int
                      , AST.lexeme = Just "10"
                      , AST.column = 2
                      }
                   )

      ASL.number (ASL.withSource "1.")
        `shouldBe` ( ASL.mkLexer 2 ""
                   , AST.Token
                      { AST.type_ = AST.Floating
                      , AST.lexeme = Just "1.0"
                      , AST.column = 2
                      }
                   )

      ASL.number (ASL.withSource "1.1")
        `shouldBe` ( ASL.mkLexer 3 ""
                   , AST.Token
                      { AST.type_ = AST.Floating
                      , AST.lexeme = Just "1.10"
                      , AST.column = 3
                      }
                   )

    it "updates the lexer accordingly and returns a token representing an negative number" $ do
      ASL.number (ASL.withSource "-100")
        `shouldBe` ( ASL.mkLexer 4 ""
                   , AST.Token
                      { AST.type_ = AST.Int
                      , AST.lexeme = Just "-100"
                      , AST.column = 4
                      }
                   )

      ASL.number (ASL.withSource "-1.")
        `shouldBe` ( ASL.mkLexer 3 ""
                   , AST.Token
                      { AST.type_ = AST.Floating
                      , AST.lexeme = Just "-1.0"
                      , AST.column = 3
                      }
                   )

      ASL.number (ASL.withSource "-1.1")
        `shouldBe` ( ASL.mkLexer 4 ""
                   , AST.Token
                      { AST.type_ = AST.Floating
                      , AST.lexeme = Just "-1.10"
                      , AST.column = 4
                      }
                   )

  describe "Arith.Syntax.Lexer.scan" $ do
    it "updates the lexer accordingly and returns the appropriate token" $ do
      ASL.scan (ASL.withSource "")
        `shouldBe` ( ASL.mkLexer 0 ""
                   , AST.Token
                      { AST.type_ = AST.End
                      , AST.lexeme = Nothing
                      , AST.column = 0
                      }
                   )

      ASL.scan (ASL.withSource "  + 10")
        `shouldBe` ( ASL.mkLexer 3 " 10"
                   , AST.Token
                      { AST.type_ = AST.Plus
                      , AST.lexeme = Nothing
                      , AST.column = 3
                      }
                   )

      mapM_
        ( \(op, token_type) ->
            ASL.scan (ASL.withSource $ op : " 10")
              `shouldBe` ( ASL.mkLexer 1 " 10"
                         , AST.Token
                            { AST.type_ = token_type
                            , AST.lexeme = Nothing
                            , AST.column = 1
                            }
                         )
        )
        $ zip
          "-/%*^=()<>"
          [ AST.Minus
          , AST.Slash
          , AST.Modulo
          , AST.Star
          , AST.Hat
          , AST.Equal
          , AST.LeftParen
          , AST.RightParen
          , AST.Less
          , AST.Greater
          ]

      mapM_
        ( \(op, token_type) ->
            ASL.scan (ASL.withSource $ op ++ " 10")
              `shouldBe` ( ASL.mkLexer 2 " 10"
                         , AST.Token
                            { AST.type_ = token_type
                            , AST.lexeme = Nothing
                            , AST.column = 2
                            }
                         )
        )
        $ zip
          ["!=", "<=", ">="]
          [AST.NotEqual, AST.LessEqual, AST.GreaterEqual]

      ASL.scan (ASL.withSource ".10")
        `shouldBe` ( ASL.mkLexer 3 ""
                   , AST.Token
                      { AST.type_ = AST.Floating
                      , AST.lexeme = Just "0.10"
                      , AST.column = 3
                      }
                   )

      ASL.scan (ASL.withSource ".-")
        `shouldBe` ( ASL.mkLexer 1 ".-"
                   , AST.Token
                      { AST.type_ = AST.Error
                      , AST.lexeme = Just "Syntax Error: Unexpected token: '-'"
                      , AST.column = 1
                      }
                   )

      ASL.scan (ASL.withSource "-10")
        `shouldBe` ( ASL.mkLexer 3 ""
                   , AST.Token
                      { AST.type_ = AST.Int
                      , AST.lexeme = Just "-10"
                      , AST.column = 3
                      }
                   )

      ASL.scan (ASL.withSource "-1")
        `shouldBe` ( ASL.mkLexer 2 ""
                   , AST.Token
                      { AST.type_ = AST.Int
                      , AST.lexeme = Just "-1"
                      , AST.column = 2
                      }
                   )

      ASL.scan (ASL.withSource "-.1")
        `shouldBe` ( ASL.mkLexer 3 ""
                   , AST.Token
                      { AST.type_ = AST.Floating
                      , AST.lexeme = Just "-0.1"
                      , AST.column = 3
                      }
                   )

      ASL.scan (ASL.withSource "-.10")
        `shouldBe` ( ASL.mkLexer 4 ""
                   , AST.Token
                      { AST.type_ = AST.Floating
                      , AST.lexeme = Just "-0.10"
                      , AST.column = 4
                      }
                   )

      ASL.scan (ASL.withSource "sin(1)")
        `shouldBe` ( ASL.mkLexer 3 "(1)"
                   , AST.Token
                      { AST.type_ = AST.Function
                      , AST.lexeme = Just "sin"
                      , AST.column = 3
                      }
                   )

      ASL.scan (ASL.withSource "cos(1)")
        `shouldBe` ( ASL.mkLexer 3 "(1)"
                   , AST.Token
                      { AST.type_ = AST.Function
                      , AST.lexeme = Just "cos"
                      , AST.column = 3
                      }
                   )

      ASL.scan (ASL.withSource "log(1)")
        `shouldBe` ( ASL.mkLexer 3 "(1)"
                   , AST.Token
                      { AST.type_ = AST.Function
                      , AST.lexeme = Just "log"
                      , AST.column = 3
                      }
                   )

      ASL.scan (ASL.withSource "point(1)")
        `shouldBe` ( ASL.mkLexer 5 "(1)"
                   , AST.Token
                      { AST.type_ = AST.Function
                      , AST.lexeme = Just "point"
                      , AST.column = 5
                      }
                   )

      ASL.scan (ASL.withSource "1")
        `shouldBe` ( ASL.mkLexer 1 ""
                   , AST.Token
                      { AST.type_ = AST.Int
                      , AST.lexeme = Just "1"
                      , AST.column = 1
                      }
                   )

      ASL.scan (ASL.withSource "a")
        `shouldBe` ( ASL.mkLexer 0 "a"
                   , AST.Token
                      { AST.type_ = AST.Error
                      , AST.lexeme = Just "Syntax Error: Unexpected token: 'a'"
                      , AST.column = 0
                      }
                   )
