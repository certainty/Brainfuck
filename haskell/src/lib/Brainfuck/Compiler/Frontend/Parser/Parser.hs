module Brainfuck.Compiler.Frontend.Parser.Parser
    (parse,
     ParseResult(..))
    where

import Data.List (foldl')
import Brainfuck.Compiler.Frontend.Lexer.Types
import Brainfuck.Compiler.Frontend.Parser.Types

type ParseContext  = [AST]
type ParseStack    = [[AST]]
type LocationStack = [Location]


-- Grammar
-- E  -> T | TS | L
-- TS -> TE
-- L  -> [E]
-- T  -> + | - | < | > | . | ,

-- TODO:
-- don't bail out after the first error, instead jot it down and continue parsing
-- then report all errors
parse :: [Token] -> ParseResult
parse tokens = parse' tokens (Location 1 1) [] [] []

-- this is basically an LL(1) parser
parse' :: [Token] -> Location -> ParseContext -> ParseStack -> LocationStack -> ParseResult
parse' [] loc context [] _           = Right (reverse context)
parse' [] loc context _  _           = Left  (ParseError "unbalanced []" loc)
parse' ((RBracket):ts) loc _ [] _    = Left  (ParseError "unexpected ]"  loc)
parse' (t:ts) loc context stack sloc =
    case t of
      Plus              -> parse' ts (incPos loc) ((IncByte loc):context) stack sloc
      Minus             -> parse' ts (incPos loc) ((DecByte loc):context) stack sloc
      Greater           -> parse' ts (incPos loc) ((IncPtr  loc):context) stack sloc
      Less              -> parse' ts (incPos loc) ((DecPtr  loc):context) stack sloc
      Dot               -> parse' ts (incPos loc) ((PutByte loc):context) stack sloc
      Comma             -> parse' ts (incPos loc) ((GetByte loc):context) stack sloc
      LBracket          -> parse' ts (incPos loc) [] (context:stack) (loc:sloc)
      RBracket          -> parse' ts (incPos loc) (Loop l (reverse context):s) tack rloc
      (Whitespace ws)   -> parse' ts (foldl' updateLoc loc ws) context stack sloc
    where (s:tack) = stack
          (l:rloc) = sloc
          incPos (Location l p)  = (Location l (1+p))
          incLine (Location l _) = (Location (1+l) 1)
          updateLoc l c = case c of
                            '\n' -> incLine l
                            _    -> incPos l
