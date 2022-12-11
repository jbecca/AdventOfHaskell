module Main where

main :: IO ()
main = do
    inputString <- Prelude.readFile "input/day7.txt"
    let l = lines inputString
    print l
