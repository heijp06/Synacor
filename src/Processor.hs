{-# LANGUAGE RecordWildCards #-}

module Processor
    ( Processor(halted, err, output)
    , processor
    , run
    ) where

import Control.Lens.At (ix)
import Control.Lens.Setter ((.~))
import Control.Monad.State (State, get, put, execState, unless, when)
import Data.Array ((!), Array)
import Data.Bits ((.&.), (.|.), complement)
import Data.Char (chr)
import Data.Function ((&))
import GHC.Arr (listArray)
import Prelude hiding (and, not, or, read)

data Processor = Processor { memory :: Array Int Int
                           , halted :: Bool
                           , instructionPointer :: Int
                           , registers :: [Int]
                           , stack :: [Int]
                           , output :: String
                           , err :: String
                           }

type ProcessorState = State Processor

memSize :: Int
memSize = 32768

numberOfRegisters :: Int
numberOfRegisters = 8

processor :: [Int] -> Processor
processor code = Processor mem False 0 (replicate 8 0) [] "" ""
    where
        mem = listArray (0, memSize - 1) $ code ++ replicate (memSize - length code) 0

run :: Processor -> Processor
run = execState doRun

doRun :: ProcessorState ()
doRun = do
    ins <- read
    execute ins
    Processor{..} <- get
    unless halted doRun

read :: ProcessorState Int
read = do
    Processor{..} <- get
    let value = memory ! instructionPointer
    put Processor { instructionPointer = instructionPointer + 1, .. }
    case value of
        _ | 0 <= value && value < memSize -> return value
        _ | memSize <= value && value < memSize + 8 -> return $ registers !! (value - memSize)
        _ -> do
                raise $ "Invalid value while reading: " ++ show value
                return (-1)

register :: ProcessorState Int
register = do
    Processor{..} <- get
    let reg = memory ! instructionPointer - memSize
    if reg >= 0 || reg < numberOfRegisters
        then do
            put Processor { instructionPointer = instructionPointer + 1, .. }
            return reg
        else do
            raise $ "Invalid register " ++ show reg
            return (-1)
setRegister :: Int -> Int -> ProcessorState ()
setRegister reg value = do
    Processor{..} <- get
    put Processor { registers = registers & ix reg .~ value, .. }

execute :: Int -> ProcessorState ()
execute 0 = halt
execute 1 = set
execute 2 = push
execute 3 = pop
execute 4 = eq
execute 5 = gt
execute 6 = jmp
execute 7 = jt
execute 8 = jf
execute 9 = add
execute 12 = and
execute 13 = or
execute 14 = not
execute 17 = call
execute 19 = out
execute 21 = noop
execute ins = raise $ "Instruction not implemented: " ++ show ins ++ "."

halt :: ProcessorState ()
halt = raise "Process halted."

set :: ProcessorState ()
set = do
    reg <- register
    value <- read
    setRegister reg value

push :: ProcessorState ()
push = do
    value <- read
    Processor{..} <- get
    put Processor { stack = value : stack, .. }

pop :: ProcessorState ()
pop = do
    reg <- register
    Processor{..} <- get
    when (null stack) $ raise "Pop from empty stack"
    put Processor { stack = tail stack, .. }
    setRegister reg $ head stack

eq :: ProcessorState ()
eq = do
    reg <- register
    arg1 <- read
    arg2 <- read
    setRegister reg $ if arg1 == arg2 then 1 else 0

gt :: ProcessorState ()
gt = do
    reg <- register
    arg1 <- read
    arg2 <- read
    setRegister reg $ if arg1 > arg2 then 1 else 0

jmp :: ProcessorState ()
jmp = do
    i <- read
    Processor{..} <- get
    put Processor { instructionPointer = i, .. }

jt :: ProcessorState ()
jt = do
    flag <- read
    ins <- read
    Processor{..} <- get
    unless (flag == 0) $ put Processor { instructionPointer = ins, .. }

jf :: ProcessorState ()
jf = do
    flag <- read
    ins <- read
    Processor{..} <- get
    when (flag == 0) $ put Processor { instructionPointer = ins, .. }

add :: ProcessorState ()
add = do
    reg <- register
    arg1 <- read
    arg2 <- read
    setRegister reg $ (arg1 + arg2) `mod` memSize

and :: ProcessorState ()
and = do
    reg <- register
    arg1 <- read
    arg2 <- read
    setRegister reg $ arg1 .&. arg2

or :: ProcessorState ()
or = do
    reg <- register
    arg1 <- read
    arg2 <- read
    setRegister reg $ arg1 .|. arg2

not :: ProcessorState ()
not = do
    reg <- register
    arg1 <- read
    setRegister reg $ complement arg1 .&. (memSize - 1)

call :: ProcessorState ()
call = do
    addr <- read
    Processor{..} <- get
    put Processor { stack = instructionPointer : stack, instructionPointer = addr, .. }

out :: ProcessorState ()
out = do
    i <- read
    Processor{..} <- get
    put Processor { output = output ++ [chr i], .. }

noop :: ProcessorState ()
noop = return ()

raise :: String -> ProcessorState ()
raise msg = do
    proc <- get
    put $ proc { err = msg, halted = True }
