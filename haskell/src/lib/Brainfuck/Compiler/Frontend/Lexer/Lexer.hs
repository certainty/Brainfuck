module Brainfuck.Compiler.Frontend.Lexer.Lexer (analyze) where
import Brainfuck.Compiler.Frontend.Lexer.Types
import Data.List (break,lookup)

keywords :: [(Char,Token)]
keywords = [('+',Plus),
            ('-',Minus),
            ('<',Less),
            ('>',Greater),
            ('.',Dot),
            (',',Comma),
            ('[',LBracket),
            (']',RBracket)]

isKeyword :: Char -> Bool
isKeyword c = c `elem` (map fst keywords)

-- lexical analysis is as simple as it can be
analyze :: [Char] -> [Token]
analyze [] = []
analyze (c:cs) = case lookup c keywords of
                   (Just val) -> val : analyze cs
                   Nothing    -> let parts = break isKeyword (c:cs)
                                 in
                                   (Whitespace (fst parts)) : analyze (snd parts)
