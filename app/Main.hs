module Main where

import qualified Data.ByteString as B
import Data.Hash.MD5 (Str(Str), md5s)
import Data.List.Split (chunksOf)
import System.Environment (getArgs)
import System.IO (openBinaryFile, IOMode (ReadMode))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [code] -> md5 code
    _ -> do
      ps <- program
      print ps

md5 :: String -> IO ()
md5 xs = print hash
  where
    hash = md5s $ Str xs

program :: IO [Int]
program = do
  h <- openBinaryFile "challenge.bin" ReadMode
  inputBytes <- map fromIntegral . B.unpack <$> B.hGetContents h
  return . map (\ xs -> head xs + 256 * xs !! 1) $ chunksOf 2 inputBytes
