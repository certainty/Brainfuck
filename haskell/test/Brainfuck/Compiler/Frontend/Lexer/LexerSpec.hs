module Brainfuck.Compiler.Frontend.Lexer.LexerSpec (spec) where

import Test.Hspec
import Brainfuck.Compiler.Frontend.Lexer.Types
import qualified Brainfuck.Compiler.Frontend.Lexer.Lexer as L

spec :: Spec
spec = do
  describe "Non-Whitespace" $ do
     it "recognizes +" $ do
       L.analyze "+" `shouldBe` [Plus]
     it "recognizes -" $ do
       L.analyze "-" `shouldBe` [Minus]
     it "recognizes ." $ do
       L.analyze "." `shouldBe` [Dot]
     it "recognizes ," $ do
       L.analyze "," `shouldBe` [Comma]
     it "recognizes <" $ do
       L.analyze "<" `shouldBe` [Less]
     it "recognizes >" $ do
       L.analyze ">" `shouldBe` [Greater]
     it "recognizes [" $ do
       L.analyze "[" `shouldBe` [LBracket]
     it "recognizes ]" $ do
       L.analyze "]" `shouldBe` [RBracket]

  describe "Whitespace" $ do
    it "recognizes single whitespace" $ do
      L.analyze " " `shouldBe` [(Whitespace " ")]
    -- make this a property check instead
    it "treats every other character as whitespace" $ do
      L.analyze "a" `shouldBe` [(Whitespace "a")]
    it "generates only one Whitespace for the entire lexeme" $ do
      L.analyze "aab   \nfoo" `shouldBe` [(Whitespace "aab   \nfoo")]

  describe "Combined" $ do
    it "recognizes a list of tokens" $ do
      L.analyze ".<>   []" `shouldBe` [Dot,Less,Greater,(Whitespace "   "),LBracket,RBracket]
