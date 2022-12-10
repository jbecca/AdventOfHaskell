module Main where

import System.Environment
import System.Exit
import Data.List

main :: IO ()

-- main = getArgs >>= parse >>= putStr . tac
main = do
    args <- getArgs
    parsedArgs <- parse args
    print $ sum (take 3 $ reverse $ sort $ maxCalories [] 0 (list_ $ parsedArgs))

tac :: String -> String
tac  = unlines . reverse . lines

toInt_ x = read x :: Integer

list_ x = lines x

whichIsGreater :: Ord a => a -> a -> a
whichIsGreater x y = if x > y then x else y

maxCalories :: [Integer] -> Integer -> [String] -> [Integer]
maxCalories x y [] = y : x 
maxCalories x y ([]:xs) = maxCalories (y : x) 0 xs
maxCalories x y (xx:xs) = maxCalories x (y + (toInt_ xx)) xs

parse ["-h"] = usage   >> exit
parse ["-v"] = version >> exit
parse []     = getContents
parse fs     = concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: tac [-vh] [file ..]"
version = putStrLn "Haskell tac 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
