module Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.StackMachine (fromIntermediate)
where

import qualified Brainfuck.Compiler.Backend.Intermediate.Types as IT
import Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.Assembly as SMA
import Data.List (foldl',concatMap)


fromIntermediate :: [IT.Intermediate] -> [SMA.TranslationUnit]
fromIntermediate = concatMap translateOne


countInstructions :: [SMA.TranslationUnit] -> Int
countInstructions = foldl' addInstructions 0
    where
      addInstructions amount (SMA.TU _ instrs) = amount + (length instrs)


translateOne :: IT.Intermediate -> [SMA.TranslationUnit]

-- NOP
translateOne (IT.Nop loc) = [SMA.TU (IT.Nop loc) [SMA.Nop]]

-- Add val
--
-- Adds the val to the current value at mp
--
-- lp      ; load memorypointer into acc
-- lw      ; load word
-- psh     ; save word
-- li val  ; load value
-- add     ; add value and word
-- psh     ; save result
-- lp      ; load memory pointer
-- sw      ; store word
translateOne (IT.Add value loc) = [SMA.TU (IT.Add value loc) [SMA.Lp,
                                                              SMA.Lw,
                                                              SMA.Psh,
                                                              (SMA.Li value),
                                                              SMA.Add,
                                                              SMA.Psh,
                                                              SMA.Lp,
                                                              (SMA.Sw)]]

-- Inc
--
-- lp         ; load memory pointer
-- psh        ; save pointer
-- li offset  ; load offset
-- add        ; add up
-- sp         ; store memory pointer
translateOne (IT.Inc offset loc) = [SMA.TU (IT.Inc offset loc) [SMA.Lp,
                                                                SMA.Psh,
                                                                (SMA.Li offset),
                                                                SMA.Add,
                                                                SMA.Sp]]

-- Put
--
-- lp
-- lw
-- out
translateOne (IT.Put loc) = [SMA.TU (IT.Put loc) [SMA.Lp,SMA.Lw,SMA.Out]]

-- Get
--
-- in
-- psh
-- lp
-- sw
translateOne (IT.Get loc) = [SMA.TU (IT.Get loc) [SMA.In,SMA.Psh,SMA.Lp,SMA.Sw]]


-- Loop
--
-- lp
-- lw
-- psh
-- li addr
-- jz
--
-- instructions ...
--
-- lp
-- lw
-- psh
-- li addr
-- jnz
translateOne (IT.Loop exps loc) =  let body = fromIntermediate exps
                                       jz   = SMA.TU (IT.Loop exps loc) [SMA.Lp,SMA.Lw,SMA.Psh,(SMA.Li $ jnzAddr body),SMA.Jz]
                                       jnz  = SMA.TU (IT.Loop exps loc) [SMA.Lp,SMA.Lw,SMA.Psh,(SMA.Li $ jzAddr body),SMA.Jnz]
                                   in [jz] ++ body ++ [jnz]
                                       where jnzAddr body = 1 + (countInstructions body)
                                             jzAddr body  = negate $ 4 + (countInstructions body) + 5
