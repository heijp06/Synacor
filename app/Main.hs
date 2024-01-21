module Main where

import Control.Monad (unless)
import qualified Data.ByteString as B
import Data.Hash.MD5 (Str(Str), md5s)
import Data.List.Split (chunksOf)
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
  unless (null xs) $ putStrLn xs
  if halted proc'
    then putStrLn $ err proc'
    else loop proc'