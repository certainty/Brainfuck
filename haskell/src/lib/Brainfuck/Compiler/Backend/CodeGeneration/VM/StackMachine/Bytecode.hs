module Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.Bytecode (toBytecode,fromBytecode)
where

import Brainfuck.Compiler.Backend.CodeGeneration.VM.StackMachine.Assembly
import Control.Monad (liftM)
import Data.Binary
import qualified Data.ByteString.Lazy as B


-- all nullary ops have its lsb set to 0
-- 0x0  - Nop
-- 0x1  - Add
-- 0x2  - Lw
-- 0x3  - Sw
-- 0x4  - Jz
-- 0x5  - Jnz
-- 0x6  - Psh
-- 0x7  - Out
-- 0x8  - In
-- 0x9  - Lp (load memory pointer)
-- 0xa  - Sp (store memory pointer)


-- the only unary op has its lsb set to 1
-- 0x10 - Li

instance Binary Instruction where
    put Nop      = put (0x0 :: Word8)
    put Add      = put (0x1 :: Word8)
    put Lw       = put (0x2 :: Word8)
    put Sw       = put (0x3 :: Word8)
    put Jz       = put (0x4 :: Word8)
    put Jnz      = put (0x5 :: Word8)
    put Psh      = put (0x6 :: Word8)
    put Out      = put (0x7 :: Word8)
    put In       = put (0x8 :: Word8)
    put Lp       = put (0x9 :: Word8)
    put Sp       = put (0xa :: Word8)
    put (Li v)   = put (0x10 :: Word8) >> put v

    get = do tag <- getWord8
             case tag of
               0x0 -> return Nop
               0x1 -> return Add
               0x2 -> return Lw
               0x3 -> return Sw
               0x4 -> return Jz
               0x5 -> return Jnz
               0x6 -> return Psh
               0x7 -> return Out
               0x8 -> return In
               0x9 -> return Lp
               0xa -> return Sp
               0x10 -> liftM Li get

toBytecode :: [Instruction] -> B.ByteString
toBytecode = encode

fromBytecode :: B.ByteString -> [Instruction]
fromBytecode = decode
