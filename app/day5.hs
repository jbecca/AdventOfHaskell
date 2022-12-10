{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.Split

main :: IO ()
main = do
    inputString <- Prelude.readFile "input/day5.txt"
    let movesToMake = map justInts $ map command' $ drop 10 $ lines inputString
    let answer = applyMove startingStack movesToMake
    print answer

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
six' :: Stack
six' = "BQWMDNHT" :: Stack
seven' :: Stack
seven' = "LSGJRBM" :: Stack
eight' :: Stack
eight' = "TRBVGWNZ" :: Stack
nine' :: Stack
nine' = "LPNDGW" :: Stack

type Move = [Int]
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

updateStacks :: (StackIndex, Stack) -> Stacks -> Stacks
updateStacks (n, newStack) inputStack = [if nn /= n then x else newStack | (nn, x) <- zip [1..] inputStack]

move' :: Move -> Stacks -> [(StackIndex, Stack)]
move' move stacks = [(a,newA), (b,newB)]
  where
    newA = drop n movedFrom
    -- PART 1: one crate at a time: reverse the crates
    --newB = (reverse (take n movedFrom)) <> movedTo
    -- PART 2: stacks moved at once: no reverse
    newB = (take n movedFrom) <> movedTo
    movedFrom = stacks !! (a - 1)
    movedTo = stacks !! (b - 1)
    n = move !! 0
    a = move !! 1
    b = move !! 2

applyMove :: Stacks -> [Move] -> Stacks
applyMove stacks [] = stacks
applyMove stacks (move:moves) = applyMove (updateStacks tup2 (updateStacks tup1 stacks)) moves
  where
    tups = move' move stacks
    tup1 = head tups
    tup2 = tups !! 1
