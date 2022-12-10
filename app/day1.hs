module Main where

import Data.List ( sort )

main :: IO ()
main = do
    inputString <- readFile "input/day1.txt"
    print $ sum (take 3 $ reverse $ sort $ maxCalories [] 0 (list_ $ inputString))

toInt_ :: String -> Integer
toInt_ x = read x :: Integer

list_ :: String -> [String]
list_ x = lines x

whichIsGreater :: Ord a => a -> a -> a
whichIsGreater x y = if x > y then x else y

maxCalories :: [Integer] -> Integer -> [String] -> [Integer]
maxCalories x y [] = y : x 
maxCalories x y ([]:xs) = maxCalories (y : x) 0 xs
maxCalories x y (xx:xs) = maxCalories x (y + (toInt_ xx)) xs
