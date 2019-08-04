module Brainfuck.Compiler.Compiler (ioCompileToByteCode,ioCompileToAssemblyText,ioCompileToAssembly)
where

import qualified Brainfuck.Compiler.Frontend.Lexer.Lexer as L
import qualified Brainfuck.Compiler.Frontend.Parser.Types as PT
import qualified Brainfuck.Compiler.Frontend.Parser.Parser as P
import qualified Brainfuck.Compiler.Backend.Intermediate.Types as IT
import qualified Brainfuck.Compiler.Backend.Intermediate.Intermediate as I
import qualified Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.StackMachine as BSM
import qualified Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.Assembly as SMA
import qualified Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.Bytecode as SMBC
import qualified Data.ByteString.Lazy as B

import Data.Char (ord)

-- parse and semantic analysis
phase1 :: String -> Either String [PT.AST]
phase1   = parse

ioPhase1 :: String -> IO [PT.AST]
ioPhase1 = ioParse

parse :: String -> Either String [PT.AST]
parse inp = case parse' inp of
              (Left err)  -> Left (translateError err)
              (Right ast) -> Right ast
    where
      parse' = (P.parse . L.analyze)
      translateError (PT.ParseError message (PT.Location line char)) = "Parse-error: " ++ message ++ " Line: " ++ show line ++ " Character: " ++ show char

ioParse :: String -> IO [PT.AST]
ioParse inp = do
  case parse inp of
    (Left err)  -> error err
    (Right ast) -> return ast

-- optimization
phase2 :: [PT.AST] -> [IT.Intermediate]
phase2 = optimize

ioPhase2 :: [PT.AST] -> IO [IT.Intermediate]
ioPhase2 = return . phase2

optimize :: [PT.AST] -> [IT.Intermediate]
optimize = I.translate

-- codegeneration
phase3 :: ([IT.Intermediate] -> B.ByteString) -> [IT.Intermediate] -> B.ByteString
phase3 = ($)

ioPhase3 :: ([IT.Intermediate] -> B.ByteString) -> [IT.Intermediate] -> IO B.ByteString
ioPhase3 backend = return . (phase3 backend)

-- HELPERS
toAssembly :: [IT.Intermediate] -> [SMA.TranslationUnit]
toAssembly = BSM.fromIntermediate

stripTranslationUnits :: [SMA.TranslationUnit] -> [SMA.Instruction]
stripTranslationUnits = concatMap strip
    where
      strip (SMA.TU _ i) = i

toBytecode :: [SMA.TranslationUnit] -> B.ByteString
toBytecode = SMBC.toBytecode . stripTranslationUnits

-- -- Basic modes of operation
-- -- Source -> Bytecode
ioCompileToByteCode :: String -> IO B.ByteString
ioCompileToByteCode inp = ioPhase1 inp >>= ioPhase2 >>= (ioPhase3 (toBytecode . toAssembly))

-- -- Source -> AssemblyText
ioCompileToAssemblyText :: String -> IO B.ByteString
ioCompileToAssemblyText inp = ioPhase1 inp >>= ioPhase2 >>= (ioPhase3 (packStr . SMA.assemble . toAssembly))
    where
      packStr = B.pack . map (fromIntegral . ord)

-- -- Source -> Assembly
ioCompileToAssembly :: String -> IO [SMA.Instruction]
ioCompileToAssembly inp = ioPhase1 inp >>= ioPhase2 >>= return . stripTranslationUnits . toAssembly
