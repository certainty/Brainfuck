module Brainfuck.Compiler.Backend.Intermediate.Types (Intermediate(..))
where

import Brainfuck.Compiler.Frontend.Parser.Types as PT

-- This intermediate repersentation
-- is a trimmed down version of the original primitives
-- basically it replaces +- and <> with a single operation each
-- which allow negative values

data Intermediate = Add Int PT.Location  -- Add a value to the current byte
                  | Put PT.Location      -- Put byte to stdout
                  | Get PT.Location      -- Read byte from stdin
                  | Inc Int PT.Location  -- increment memory pointer (negative offsets supported)
                  | Nop PT.Location      -- NOOP
                  | Loop [Intermediate] PT.Location
                    deriving (Show,Eq)
