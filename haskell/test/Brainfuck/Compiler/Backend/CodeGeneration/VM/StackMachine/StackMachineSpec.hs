module Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.StackMachineSpec (spec) where

import Test.Hspec
import qualified Brainfuck.Compiler.Frontend.Parser.Types as PT
import qualified Brainfuck.Compiler.Backend.Intermediate.Types as IT
import qualified Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.Assembly as SMA
import qualified Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.StackMachine as SM

loc = PT.Location 1 1

spec :: Spec
spec = do
  describe "translation from intermediate" $ do
    it "translates Add" $ do
       SM.fromIntermediate [IT.Add 2 loc] `shouldBe` [SMA.TU (IT.Add 2 loc) [SMA.Lp,
                                                                             SMA.Lw,
                                                                             SMA.Psh,
                                                                             SMA.Li 2,
                                                                             SMA.Add,
                                                                             SMA.Psh,
                                                                             SMA.Lp,
                                                                             SMA.Sw]]
    it "translates Inc" $ do
       SM.fromIntermediate [IT.Inc 2 loc] `shouldBe` [SMA.TU (IT.Inc 2 loc) [SMA.Lp,
                                                                             SMA.Psh,
                                                                             SMA.Li 2,
                                                                             SMA.Add,
                                                                             SMA.Sp]]

    it "translates Get" $ do
       SM.fromIntermediate [IT.Get loc] `shouldBe` [SMA.TU (IT.Get loc) [SMA.In,
                                                                         SMA.Psh,
                                                                         SMA.Lp,
                                                                         SMA.Sw]]
    it "translates Put" $ do
       SM.fromIntermediate [IT.Put loc] `shouldBe` [SMA.TU (IT.Put loc) [SMA.Lp,
                                                                         SMA.Lw,
                                                                         SMA.Out]]
    it "translates Nop" $ do
       SM.fromIntermediate [IT.Nop loc] `shouldBe` [SMA.TU (IT.Nop loc) [SMA.Nop]]

    it "translates Loop" $ do
       SM.fromIntermediate [IT.Loop [IT.Nop loc] loc] `shouldBe` [SMA.TU (IT.Loop [IT.Nop loc] loc) [SMA.Lp,
                                                                                                     SMA.Lw,
                                                                                                     SMA.Psh,
                                                                                                     SMA.Li 2,
                                                                                                     SMA.Jz],
                                                                  SMA.TU (IT.Nop loc) [SMA.Nop],
                                                                  SMA.TU (IT.Loop [IT.Nop loc] loc) [SMA.Lp,
                                                                                                     SMA.Lw,
                                                                                                     SMA.Psh,
                                                                                                     SMA.Li (-10),
                                                                                                     SMA.Jnz]]
