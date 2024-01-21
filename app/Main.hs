{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (unless, when)
import qualified Data.ByteString as B
import Data.Hash.MD5 (Str(Str), md5s)
import Data.List.Split (chunksOf)
import System.Console.ANSI (setSGR, SGR (SetColor, Reset), ConsoleLayer (Foreground), ColorIntensity (Vivid), Color (Yellow, Red))
import System.Environment (getArgs)
import System.IO (openBinaryFile, IOMode (ReadMode))

import Processor (Processor(..), inputPending, processor, run, setInput)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [code] -> print . md5s $ Str code
    _ -> do
      ps <- program
      loop $ processor ps

program :: IO [Int]
program = do
  h <- openBinaryFile "challenge.bin" ReadMode
  inputBytes <- map fromIntegral . B.unpack <$> B.hGetContents h
  return . map (\ xs -> head xs + 256 * xs !! 1) $ chunksOf 2 inputBytes

loop :: Processor -> IO ()
loop proc = do
  -- dump proc
  let proc' = run proc
  let xs = output proc'
  setSGR [SetColor Foreground Vivid Yellow]
  unless (null xs) $ putStrLn xs
  setSGR [Reset]
  if halted proc'
    then do
      setSGR [SetColor Foreground Vivid Red]
      let msg = err proc'
      putStrLn msg
      setSGR [Reset]
      when (msg /= inputPending) $ return ()
      putChar '\n'
      line <- getLine
      loop $ setInput proc' (line ++ "\n")
    else loop proc'
  
dump :: Processor -> IO ()
dump Processor{..} = do
  putChar '\n'
  putStrLn $ replicate 80 '='
  putStr "Halted:\t\t"
  print halted
  putStr "IP:\t\t"
  print instructionPointer
  putStr "Registers:\t"
  print registers
  putStr "Stack:\t\t"
  print stack
  putStr "Input:\t\t"
  print input
  putStr "Output:\t\t"
  print output
  putStr "Error:\t\t"
  print err
  putStrLn $ replicate 80 '='
  putChar '\n'