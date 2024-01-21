{-# LANGUAGE RecordWildCards #-}

module Processor
    ( Processor(halted, err, output)
    , processor
    , run
    ) where

import Control.Monad.State (State, get, put, execState, unless)
import Data.Array ((!), Array)
import Data.Char (chr)
import GHC.Arr (listArray)
import Prelude hiding (read)

data Processor = Processor { memory :: Array Int Int
                           , halted :: Bool
                           , instructionPointer :: Int
                           , output :: String
                           , err :: String
                           }

type ProcessorState = State Processor

processor :: [Int] -> Processor
processor code = Processor mem False 0 "" ""
    where
        mem = listArray (0, 32767) $ code ++ replicate (32768 - length code) 0

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
    return value

execute :: Int -> ProcessorState ()
execute 0 = halt
execute 6 = jmp
execute 19 = out
execute 21 = noop
execute ins = raise $ "Instruction not implemented: " ++ show ins ++ "."

halt :: ProcessorState ()
halt = raise "Process halted."

jmp :: ProcessorState ()
jmp = do
    i <- read
    Processor{..} <- get
    put Processor { instructionPointer = i, .. }

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
