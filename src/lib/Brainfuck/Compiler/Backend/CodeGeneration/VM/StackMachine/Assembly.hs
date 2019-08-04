module Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.Assembly (TranslationUnit(..),assemble,Instruction(..),Mnemonic,toMnemonic)
where

import Brainfuck.Compiler.Frontend.Parser.Types as PT
import qualified Brainfuck.Compiler.Backend.Intermediate.Types as IT
import Data.List (foldl')

data Instruction = Nop
                 | Li Int -- Load Immediate
                 | Lp     -- Load memory pointer
                 | Sp     -- Store memory pointer
                 | Add    -- Add
                 | Psh    -- Push
                 | Lw     -- Load Word
                 | Sw     -- Store Word
                 | Jz     -- Jump if zero
                 | Jnz    -- Jump if not zero
                 | Out    -- Write output
                 | In     -- Read input
                   deriving (Show,Eq)

-- This allows us to map the assembly back
-- to intermediate code along with source information (which will allow us to go all the way back to the source)
data TranslationUnit = TU IT.Intermediate [Instruction] deriving (Show,Eq)

type Mnemonic = String

toMnemonic :: Instruction -> Mnemonic
toMnemonic Nop      = "nop"
toMnemonic Add      = "add"
toMnemonic (Li val) = "li " ++ (show val)
toMnemonic Lp       = "lp"
toMnemonic Sp       = "sp"
toMnemonic Psh      = "psh"
toMnemonic Lw       = "lw"
toMnemonic Sw       = "sw"
toMnemonic Jz       = "jz"
toMnemonic Jnz      = "jnz"
toMnemonic Out      = "out"
toMnemonic In       = "in"


assemble :: [TranslationUnit] -> String
assemble = foldl' assembleUnit ""

assembleUnit :: String -> TranslationUnit -> String
assembleUnit code (TU im (instr:rest)) = let code'  = code ++ "\n" ++ (toMnemonic instr) ++ "\t" ++ (comment im)
                                             in
                                               code' ++ unlines (map toMnemonic rest) ++ "\n"

location :: PT.Location -> String
location (PT.Location l p) = " - Line: " ++ (show l) ++ " Pos: " ++ (show p)

wrap :: String -> PT.Location -> String
wrap cont loc = "; " ++ (show cont) ++ (location loc) ++ "\n"

comment :: IT.Intermediate -> String
-- currently v is always 1
comment (IT.Add v loc) = if v < 0 then (wrap "-" loc) else (wrap "+" loc)
-- currently v is always 1
comment (IT.Inc  v loc) = if v < 0 then (wrap "<" loc) else (wrap ">" loc)
comment (IT.Get  loc)   = (wrap "," loc)
comment (IT.Put  loc)   = (wrap "." loc)
comment (IT.Nop  loc)   = (wrap "NOP" loc)
comment (IT.Loop _ loc) = (wrap "[]" loc)
