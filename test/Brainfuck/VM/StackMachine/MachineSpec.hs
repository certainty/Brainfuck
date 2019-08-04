module Brainfuck.VM.StackMachine.MachineSpec (spec) where

import Test.Hspec

-- import qualified Brainfuck.Compiler.Compiler as C
-- import qualified Brainfuck.VM.StackMachine.Machine as Machine
-- import qualified Data.IntMap as IMap

-- compileProgram :: String -> Machine.Program
-- compileProgram text = let (Right instrs) = C.compileToAssembly text in
--                       Machine.programFromList instrs

-- testMachine :: String -> Machine.MachineState -> IO Machine.MachineState
-- testMachine programText state = do
--   let program = compileProgram programText
--   (Right (state,_,_)) <- Machine.execVM program state Machine.runProgram
--   return state

-- theMachine = Machine.initMachine 10
-- theMachine' s = theMachine{ Machine.vmStack = s }

-- byteAt :: Machine.MachineState -> Int -> Maybe Int
-- byteAt state idx = IMap.lookup idx (Machine.vmData state)

spec :: Spec
spec = it "stubs" $ do{ True `shouldBe` True }

-- spec = do
--   describe "Running instructions" $ do
--     context "+" $ do
--        it "preserves the stack" $ do
--          result <- testMachine "+" (theMachine' [1])
--          Machine.vmStack result `shouldBe` [1]

--        it "increments the byte" $ do
--          result <-  testMachine "+" theMachine
--          byteAt result 0 `shouldBe` Just 1

--     context "-" $ do
--        it "preserves the stack" $ do
--          result <- testMachine "-" (theMachine' [1])
--          Machine.vmStack result `shouldBe` [1]

--        it "decrements the byte" $ do
--          result <- testMachine "+++-" theMachine
--          byteAt result 0 `shouldBe` Just 2

--     context ">" $ do
--        it "preserves the stack" $ do
--          result <- testMachine ">+" (theMachine' [1])
--          Machine.vmStack result `shouldBe` [1]

--        it "increments the pointer" $ do
--          result <- testMachine ">+" theMachine
--          byteAt result 1 `shouldBe` Just 1

--     context "<" $ do
--        it "preserves the stack" $ do
--          result <- testMachine ">><+" (theMachine' [1])
--          Machine.vmStack result `shouldBe` [1]

--        it "decrements the pointer" $ do
--          result <- testMachine ">>><+" theMachine
--          byteAt result 2 `shouldBe` Just 1

--     context "with loops" $ do
--        context "that are empty" $ do
--          it "preserves the stack" $ do
--            result <- testMachine "[]" (theMachine' [1])
--            Machine.vmStack result `shouldBe` [1]
--          it "terminates when the loop is not entered" $ do
--            result <- testMachine "[]" theMachine
--            let regs  = Machine.vmRegs theMachine
--                regs' = regs{ rIp = 8 }
--            result `shouldBe` theMachine{ Machine.vmRegs = regs' }

--        context "that are not empty" $ do
--          it "terminates when the loop is entered" $ do
--            result <- testMachine "[+]" theMachine
--            result `shouldBe` theMachine{ Machine.vmIp = 14 }
--          it "works for complex loops" $ do
--            -- the code moves the value of cell0 which is set initially to 1 to cell2
--            result <- testMachine "+>>[-]<<[->>+<<]" theMachine
--            byteAt result 2 `shouldBe` Just 1

--     -- context "with errors" $ do
