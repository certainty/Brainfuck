module Brainfuck.VM.StackMachine.Testbed () --(interprete,compileJIT,printLog)
where

-- import Control.Monad
-- import qualified Brainfuck.Compiler.Compiler as C
-- import qualified Brainfuck.VM.StackMachine.Machine as Machine
-- import qualified Brainfuck.VM.StackMachine.Assembly as A
-- import Data.Array.IArray
-- import qualified Data.DList as DL

-- -- TODO:
-- -- somehow the simple program "," doesn't compile or execute correctly
-- -- The stack is not empty after the program has finished. There is superfluous push somewhere


-- runProgram program = Machine.execVM program (Machine.initMachine 100) Machine.runProgram

-- interprete :: String -> (IO Machine.LogMessages)
-- interprete inp = do
--   let program = compileJIT inp
--   case program of
--     (Left e) -> print e >> return DL.empty
--     (Right program) -> do
--                    result <- runProgram program
--                    case result of
--                      (Left ((Machine.VMError e),_,logs))  -> print e >> return logs
--                      (Right s,logs,_)                   -> return logs

-- printError :: Machine.VMError -> IO ()
-- printError (Machine.VMError e) = print e

-- compileJIT :: String -> Either String Machine.Program
-- compileJIT inp = C.compileToAssembly inp >>= (\p -> return (programFromList p))

-- programFromList :: [A.Instruction] -> Machine.Program
-- programFromList instrs = listArray (0,(length instrs) - 1) instrs


-- printLog :: Machine.LogMessages -> IO ()
-- printLog logs = putStr loglines
--     where loglines = unlines $ DL.toList logs
