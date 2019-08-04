{-# LANGUAGE FlexibleInstances  #-}
module Brainfuck.Compiler.Frontend.Parser.ParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Brainfuck.Compiler.Frontend.Lexer.Types as LT
import Brainfuck.Compiler.Frontend.Parser.Types
import qualified Brainfuck.Compiler.Frontend.Parser.Parser as P

-- just to write less
loc = Location

spec :: Spec
spec = do
  describe "Parsing without errors" $ do
    it "succeeds with the parsetree" $ do
      P.parse [LT.Dot,LT.Dot,LT.Plus] `shouldBe` (Right [(PutByte $ loc 1 1) ,(PutByte $ loc 1 2),(IncByte $ loc 1 3)])
    it "parses Dot" $ do
      P.parse [LT.Dot] `shouldBe` (Right [PutByte $ loc 1 1])
    it "parses Comma" $ do
      P.parse [LT.Comma] `shouldBe` (Right [GetByte $ loc 1 1])
    it "parses Greater" $ do
      P.parse [LT.Greater] `shouldBe` (Right [IncPtr $ loc 1 1])
    it "parses Less" $ do
      P.parse [LT.Less] `shouldBe` (Right [DecPtr $ loc 1 1])
    it "parses Plus" $ do
      P.parse [LT.Plus] `shouldBe` (Right [IncByte $ loc 1 1])
    it "parses Minus" $ do
      P.parse [LT.Minus] `shouldBe` (Right [DecByte $ loc 1 1])
    it "parses simple loop" $ do
      P.parse [LT.LBracket,LT.Dot,LT.RBracket] `shouldBe` (Right [(Loop (loc 1 1) [PutByte $ loc 1 2])])
    it "parses nested loop" $ do
      P.parse [LT.LBracket,LT.Dot,LT.LBracket,LT.Minus,LT.RBracket,LT.RBracket] `shouldBe` (Right [(Loop (loc 1 1) [PutByte (loc 1 2) , (Loop (loc 1 3) [DecByte (loc 1 4)])])])

  describe "Parsing with errors" $ do
    it "detects missing closing bracket" $ do
      P.parse [LT.LBracket,LT.Dot] `shouldBe` (Left (ParseError "unbalanced []" (Location 1 3)))
    it "detects missing opening bracket" $ do
      P.parse [LT.Dot,LT.RBracket] `shouldBe` (Left (ParseError "unexpected ]" (Location 1 2)))
    it "tracks the linenumber when the return is preceded by some other whitespace" $ do
      P.parse [(LT.Whitespace "\n"),LT.Dot,LT.RBracket] `shouldBe` (Left (ParseError "unexpected ]" (Location 2 2)))
    it "tracks the linenumber when the return is preceded by some other whitespace" $ do
      P.parse [(LT.Whitespace "foo\n"),LT.Dot,LT.RBracket] `shouldBe` (Left (ParseError "unexpected ]" (Location 2 2)))
