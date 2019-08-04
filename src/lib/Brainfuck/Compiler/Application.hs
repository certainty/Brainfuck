
module Brainfuck.Compiler.Application (runApplication,parse) where

import qualified Brainfuck.Compiler.Compiler as C

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

data Flag =  Assembly              -- -a .. stop after assembly
             | Help                -- --help
               deriving (Eq,Ord,Enum,Show,Bounded)

flags = [Option ['a'] []       (NoArg Assembly)
            "Stop compilation after generation of assembly. Will produce a .assembly file"
       ,Option []    ["help"] (NoArg Help)
            "Print this help message"
       ]

parse argv = case getOpt Permute flags argv of
               (args,fs,[]) -> do
                 let files = if null fs then ["-"] else fs
                 if Help `elem` args
                 then do hPutStrLn stderr (usageInfo header flags)
                         exitWith ExitSuccess
                 else return (nub (concatMap set args), files)
               (_,_,errs)      -> do
                 hPutStrLn stderr (concat errs ++ usageInfo header flags)
                 exitWith (ExitFailure 1)
        where header = "Usage: bfc [-oh] [file ...]"
              set f      = [f]

compileFile :: String -> String -> [Flag] -> IO ()
compileFile inputPath outputPath args = do
    input    <- readFile inputPath
    bytecode <- C.ioCompileToByteCode input
    B.writeFile outputPath (C.compress bytecode)

runApplication :: IO ()
runApplication = do
  (args,files) <- (getArgs >>= parse)
  case files of
    []    -> print "You need to supply at least one file to compile"
    (f:_) -> compileFile f (f ++ ".bfc") args
  return ()
