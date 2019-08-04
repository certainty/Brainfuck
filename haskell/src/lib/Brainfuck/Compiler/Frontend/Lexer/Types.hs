module Brainfuck.Compiler.Frontend.Lexer.Types
    (Token(..),Lexeme) where

type Lexeme = String
data Token  = Plus
            | Minus
            | Greater
            | Less
            | Dot
            | Comma
            | LBracket
            | RBracket
            | Whitespace Lexeme deriving (Eq,Show)
