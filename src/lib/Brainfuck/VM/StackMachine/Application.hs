module Brainfuck.VM.StackMachine.Application (runApplication) where

import qualified Brainfuck.VM.StackMachine.Machine as SM
import qualified Brainfuck.Compiler.Compiler as C
import qualified Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.Bytecode as BC
import qualified Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.Assembly as SMA

import Control.Monad
import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import qualified Data.ByteString.Lazy as B
import qualified Codec.Compression.GZip as C

-- TODO: add MemorySize option
data Flag =  Help deriving (Eq,Ord,Enum,Show,Bounded)

flags = [Option ['h'] ["help"] (NoArg Help) "Print this help"]


parse argv = case getOpt Permute flags argv of
               (args,fs,[]) -> do
                 let files = if null fs then ["-"] else fs
                 if Help `elem` args
                 then do hPutStrLn stderr (usageInfo header flags)
                         exitWith ExitSuccess
                 else return (nub (concatMap set args), files)
               (_,_,errs)  -> do
                 hPutStrLn stderr (concat errs ++ usageInfo header flags)
                 exitWith (ExitFailure 1)
    where header = "Usage: bfv [-h] file"
          set f  = [f]

loadProgram :: String -> IO SM.Program
loadProgram path = do
  instructions <- fmap (BC.fromBytecode . C.decompress) $ B.readFile path
  -- print ("Loaded instructions" ++ (show $ map A.toString instructions))
  return $ SM.programFromList instructions

loadAndRunProgram :: String -> IO ()
loadAndRunProgram path = do
  program <- loadProgram path
  result  <- SM.execVM program (SM.initMachine 30000) SM.runProgram
  case result of
    (Right (_,_,_))   -> return ()
    (Left  (err,_,_)) -> hPutStrLn stderr (show err) >> exitWith (ExitFailure 2)

runProgram :: SM.Program -> IO ()
runProgram program = do
  SM.execVM program (SM.initMachine 30000) SM.runProgram
  return ()

runProgramSingle :: SM.Program -> IO ()
runProgramSingle program = do
  SM.execVM program (SM.initMachine 3000) SM.runProgramSingle
  return ()

-- loads source from stdin, compiles to assembly and runs the assembly on the vm
interpreteProgram :: String -> IO ()
interpreteProgram path = do
  code     <- readFile path
  assembly <- C.ioCompileToAssembly code
  runProgram $ SM.programFromList assembly

runApplication :: IO ()
runApplication = do
  (args,files) <- (getArgs >>= parse)
  case files of
    []    -> print "You need to supply a filename"
    [f]   -> loadAndRunProgram f
    -- [f]   -> interpreteProgram f
    (f:_) -> print "Multiple files given. Will only execute the first one" >> loadAndRunProgram f
