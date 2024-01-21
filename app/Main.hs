module Main where

import Control.Monad (unless)
import qualified Data.ByteString as B
import Data.Hash.MD5 (Str(Str), md5s)
import Data.List.Split (chunksOf)
import System.Console.ANSI (setSGR, SGR (SetColor, Reset), ConsoleLayer (Foreground), ColorIntensity (Vivid), Color (Yellow, Red))
import System.Environment (getArgs)
import System.IO (openBinaryFile, IOMode (ReadMode))
import Processor (Processor(..), processor, run)

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
  let proc' = run proc
  let xs = output proc'
  setSGR [SetColor Foreground Vivid Yellow]
  unless (null xs) $ putStrLn xs
  setSGR [Reset]
  if halted proc'
    then do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ err proc'
      setSGR [Reset]
    else loop proc'