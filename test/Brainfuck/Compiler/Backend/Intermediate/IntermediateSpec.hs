module Brainfuck.Compiler.Backend.Intermediate.IntermediateSpec (spec) where

import Test.Hspec

import qualified Brainfuck.Compiler.Backend.Intermediate.Types as IT
import qualified Brainfuck.Compiler.Frontend.Parser.Types as PT
import qualified Brainfuck.Compiler.Backend.Intermediate.Intermediate as I

loc = PT.Location 1 1

spec :: Spec
spec = do
  describe "Translate" $ do
    it "translates IncPtr" $ do
      I.translate [PT.IncPtr loc] `shouldBe` [IT.Inc 1 loc]
    it "translates DecPtr" $ do
      I.translate [PT.DecPtr loc] `shouldBe` [IT.Inc (-1) loc]
    it "translates IncByte" $ do
      I.translate [PT.IncByte loc] `shouldBe` [IT.Add 1 loc]
    it "translates DecByte" $ do
      I.translate [PT.DecByte loc] `shouldBe` [IT.Add (-1) loc]
    it "translates PutByte" $ do
      I.translate [PT.PutByte loc] `shouldBe` [IT.Put loc]
    it "translates GetByte" $ do
      I.translate [PT.GetByte loc] `shouldBe` [IT.Get loc]
    it "translates Loop" $ do
      I.translate [PT.Loop loc [PT.IncPtr loc]] `shouldBe` [IT.Loop [IT.Inc 1 loc] loc]
    it "translates an entire AST" $ do
      I.translate [PT.GetByte loc,PT.Loop loc [PT.IncPtr loc,PT.DecByte loc]] `shouldBe` [IT.Get loc,IT.Loop [IT.Inc 1 loc,IT.Add (-1) loc] loc]
