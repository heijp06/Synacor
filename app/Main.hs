module Main where

import Data.Hash.MD5 (Str(Str), md5s)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [code] -> md5 code
    _ -> return ()

md5 :: String -> IO ()
md5 xs = print hash
  where
    hash = md5s $ Str xs