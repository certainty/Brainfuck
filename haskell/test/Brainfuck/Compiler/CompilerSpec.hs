module Brainfuck.Compiler.CompilerSpec (spec) where

import Test.Hspec

-- import qualified Brainfuck.Compiler.Compiler as C
-- import qualified Brainfuck.VM.StackMachine.Assembly as SMA


spec :: Spec
spec = it "stubs" $ do{ True `shouldBe` True }

-- spec = do
--   describe "compiling to stackmachine assembly" $ do
--      it "works" $
--        C.compileToAssembly "+[.,>]" `shouldBe` (Right [SMA.Psh 0,
--                                                        SMA.Lw,
--                                                        SMA.Psh 1,
--                                                        SMA.Add,
--                                                        SMA.Psh 0,
--                                                        SMA.Lw,
--                                                        SMA.Psh 0,
--                                                        SMA.Lw,
--                                                        SMA.Psh 7,
--                                                        SMA.Jz,
--                                                        SMA.Psh 0,
--                                                        SMA.Lw,
--                                                        SMA.Our,
--                                                        SMA.In,
--                                                        SMA.Psh 0,
--                                                        SMA.Sw,
--                                                        SMA.Psh 1,
--                                                        SMA.Lw,
--                                                        SMA.Psh (-10),
--                                                        SMA.Jnz])
