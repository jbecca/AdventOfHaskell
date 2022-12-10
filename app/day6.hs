module Main where

import Data.List ( nub )
main :: IO ()
main = do
    inputString <- Prelude.readFile "input/day6.txt"
    let l = lines inputString
    let answer = decode' 0 $ concat l
    print answer

allUnique :: String -> Bool
allUnique s = s == nub s

markerNumber :: Int
-- markerNumber = 4 -- Part 1
markerNumber = 14 -- Part 2
decode' :: Int -> String -> Int
decode' _ [] = 0
decode' n str = if allUnique $ take markerNumber str then n + markerNumber else decode' (n+1) (drop 1 str)
