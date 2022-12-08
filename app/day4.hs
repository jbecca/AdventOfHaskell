{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.Split

main :: IO ()

main = do
    inputString <- Prelude.readFile "day4.txt"
    let t = (answer . flatList) $ lines inputString
    print t

twoLists :: String -> [String]
twoLists = splitOn ","

minMax :: String -> [String]
minMax = splitOn "-"

minMax' :: String -> [[String]]
minMax' x = map minMax $ twoLists x

toInt' :: [String] -> [Int]
toInt' x = [read i :: Int | i <- x]

isInOther :: Int -> Int -> Int -> Int -> Int
isInOther a b c d
  | a <= c, b >= d = 1
  | c <= a, d >= b = 1
  | otherwise = 0

isInOther' :: Int -> Int -> Int -> Int -> Int
isInOther' a b c d
  | b < c = 0
  | d < a = 0
  | otherwise = 1

flatList :: [String] -> [Int]
flatList x = concatMap toInt' (concatMap minMax' x)

answer :: [Int] -> Int
answer [] = 0
answer [_] = 0
answer [_,_] = 0
answer [_,_,_] = 0
answer (a:b:c:d:[]) = isInOther' a b c d
answer (a:b:c:d:xs) = isInOther' a b c d + answer xs
