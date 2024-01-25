module Decompiler
    ( decompile
    ) where

import Data.Char (chr)

decompile :: [Int] -> [String]
decompile = snd . foldl combineDecompile ((0, 0), [])

combineDecompile :: ((Int, Int), [String]) -> Int -> ((Int, Int), [String])
combineDecompile ((0, line), result) 0 = ((0, line + 1), result ++ [show line ++ " halt"])
combineDecompile ((0, line), result) 1 = ((2, line + 1), result ++ [show line ++ " set"])
combineDecompile ((0, line), result) 2 = ((1, line + 1), result ++ [show line ++ " push"])
combineDecompile ((0, line), result) 3 = ((1, line + 1), result ++ [show line ++ " pop"])
combineDecompile ((0, line), result) 4 = ((3, line + 1), result ++ [show line ++ " eq"])
combineDecompile ((0, line), result) 5 = ((3, line + 1), result ++ [show line ++ " gt"])
combineDecompile ((0, line), result) 6 = ((1, line + 1), result ++ [show line ++ " jmp"])
combineDecompile ((0, line), result) 7 = ((2, line + 1), result ++ [show line ++ " jt"])
combineDecompile ((0, line), result) 8 = ((2, line + 1), result ++ [show line ++ " jf"])
combineDecompile ((0, line), result) 9 = ((3, line + 1), result ++ [show line ++ " add"])
combineDecompile ((0, line), result) 10 = ((3, line + 1), result ++ [show line ++ " mult"])
combineDecompile ((0, line), result) 11 = ((3, line + 1), result ++ [show line ++ " mod"])
combineDecompile ((0, line), result) 12 = ((3, line + 1), result ++ [show line ++ " and"])
combineDecompile ((0, line), result) 13 = ((3, line + 1), result ++ [show line ++ " or"])
combineDecompile ((0, line), result) 14 = ((2, line + 1), result ++ [show line ++ " not"])
combineDecompile ((0, line), result) 15 = ((2, line + 1), result ++ [show line ++ " rmem"])
combineDecompile ((0, line), result) 16 = ((2, line + 1), result ++ [show line ++ " wmem"])
combineDecompile ((0, line), result) 17 = ((1, line + 1), result ++ [show line ++ " call"])
combineDecompile ((0, line), result) 18 = ((0, line + 1), result ++ [show line ++ " ret"])
combineDecompile ((0, line), result) 19 = ((1, line + 1), result ++ [show line ++ " out"])
combineDecompile ((0, line), result) 20 = ((1, line + 1), result ++ [show line ++ " in"])
combineDecompile ((0, line), result) 21 = ((0, line + 1), result ++ [show line ++ " noop"])
combineDecompile ((0, line), result) n = ((0, line + 1), result ++ [show line ++ " " ++ show n])
combineDecompile ((n, line), result) value = ((n - 1, line + 1), init result ++ [last result ++ format (last result) value])

format :: String -> Int -> String
format command n | n > 31 && n < 128 && last command == 't' && (last . init) command == 'u' = " " ++ [chr n]
format _ n | n < 32768 = " " ++ show n
format _ n | n < 32776 = " reg" ++ show (n - 32768)
format _ n = error "Illegal value: " ++ show n
