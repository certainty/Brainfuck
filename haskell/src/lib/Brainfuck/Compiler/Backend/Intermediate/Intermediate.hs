module Brainfuck.Compiler.Backend.Intermediate.Intermediate (translate)
where

import qualified Brainfuck.Compiler.Backend.Intermediate.Types as IT
import qualified Brainfuck.Compiler.Frontend.Parser.Types as PT

translate :: [PT.AST] -> [IT.Intermediate]
translate nodes = nodes >>= translateNode

translateNode :: PT.AST -> [IT.Intermediate]
translateNode (PT.IncPtr loc)    = [IT.Inc 1 loc]
translateNode (PT.DecPtr loc)    = [IT.Inc (-1) loc]
translateNode (PT.IncByte loc)   = [IT.Add 1 loc]
translateNode (PT.DecByte loc)   = [IT.Add (-1) loc]
translateNode (PT.PutByte loc)   = [IT.Put loc]
translateNode (PT.GetByte loc)   = [IT.Get loc]
translateNode (PT.Loop loc exps) = [IT.Loop (translate exps) loc]
