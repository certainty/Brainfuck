module Brainfuck.Compiler.Frontend.Parser.Types
    (AST(..),
     ParseError(..),
     Location(..),
     Line,
     Position,
     ParseResult(..)
    ) where

data AST = IncPtr Location
         | DecPtr Location
         | IncByte Location
         | DecByte Location
         | PutByte Location
         | GetByte Location
         | Loop Location [AST] deriving (Eq,Show)

type Line         = Int
type Position     = Int
data Location     = Location Line Position deriving (Eq,Show)
data ParseError   = ParseError String Location deriving (Eq,Show)
type ParseResult  = Either ParseError [AST]
