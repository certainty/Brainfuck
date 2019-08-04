module Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.BytecodeSpec (spec) where

import Test.Hspec
import Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.Assembly
import Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.Bytecode


spec :: Spec

spec = do
  describe "encode / decode" $ do
    it "are two inverse operations" $ do
      (fromBytecode . toBytecode $ [Nop,Add]) `shouldBe` [Nop,Add]
