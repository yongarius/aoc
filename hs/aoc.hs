import Prelude
import Data.Char
import System.IO
import Control.Monad


main :: IO()
main = do
        handle <- openFile "aoc1_input.txt" ReadMode
        contents <- hGetContents handle
        let input = contents
        print $ appendHead input
        print (sumMatchingDigits input)
        hClose handle


sumMatchingDigits :: [Char] -> Int
sumMatchingDigits xs = sumMatchDigits (appendHead xs)

sumMatchDigits :: [Char] -> Int
sumMatchDigits (x:xs)
    | xs == [] = 0
    | x == head xs = (read [x] :: Int) + sumMatchDigits xs
    | otherwise = sumMatchDigits xs
sumMatchDigits x = 0

compareDigits :: Int -> Int -> Int
compareDigits x y
    | x == y = x
    |otherwise = 0

appendHead :: [Char] -> [Char]
appendHead xs = (init xs) ++ [head xs]
