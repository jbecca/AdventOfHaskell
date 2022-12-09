{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.Split

main :: IO ()
main = do
    inputString <- Prelude.readFile "day5.txt"
    let t = lines inputString
    print t

one' :: Stack
one' = "DTWNL" :: Stack

two' :: Stack
two' = "HPC" :: Stack

three' :: Stack
three' = "JMGDNHPW" :: Stack

four' :: Stack
four' = "LQTNSWC" :: Stack

five' :: Stack
five' = "NCHP" :: Stack

six' = "BQWMDNHT" :: Stack
seven' = "LSGJRBM" :: Stack
eight' = "TRBVGWNZ" :: Stack
nine' = "LPNDGW" :: Stack

type StackIndex = Int
type Stack = String
type Stacks = [String]

startingStack :: Stacks
startingStack = [
  one',
  two',
  three',
  four',
  five',
  six',
  seven',
  eight',
  nine'
                ]

command' :: [Char] -> [[Char]]
command' = splitOn " "

justInts :: [[Char]] -> [Int]
justInts [_,a,_,b,_,c] = [read a,read b,read c]

updateStacks :: Stacks -> (StackIndex, Stack) -> Stacks
updateStacks inputStack (n, newStack) = [if nn /= n then x else newStack | (nn, x) <- zip [1..] inputStack]

move' :: Int -> StackIndex -> StackIndex -> Stacks -> Stacks
move' n a b stacks = [newA, newB]
  where
    newA = drop n movedFrom
    newB = (reverse (take n movedFrom)) <> movedTo
    movedFrom = stacks !! a
    movedTo = stacks !! b
