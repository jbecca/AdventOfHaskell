{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Data.Text.IO
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment
import System.Exit
import Data.List
import Data.Void
import Data.Maybe

main :: IO ()

-- main = getArgs >>= parse >>= putStr . tac
main = do
    args <- getArgs
    inputString <- Prelude.readFile "txt.txt"
    let t = lines inputString
    let a = sum $ map priority $ answer' t
    print (a)

firstHalf :: [Char] -> [Char]
firstHalf input = take ((length input) `div` 2) input

secondHalf :: [Char] -> [Char]
secondHalf input = drop ((length input) `div` 2) input

sameInTwoElem :: [Char] -> [Char] -> Char
sameInTwoElem x y = head [z | z <- x, z `elem` y]

sameInThreeElem :: [Char] -> [Char] -> [Char] -> Char
sameInThreeElem x y z = head [element | element <- x, element `elem` y, element `elem` z]

priority :: Char -> Int
priority x = 1 + (fromJust (elemIndex x (['a'..'z'] ++ ['A'..'Z'])))

answer :: [[Char]] -> Int
answer inputString = sum $ map priority [sameInTwoElem (firstHalf x) (secondHalf x) | x <- inputString]

answer' :: [[Char]] -> [Char]
answer' (x:y:z:[]) = [sameInThreeElem x y z]
answer' (x:y:z:xs) = (sameInThreeElem x y z):(answer' xs)
-- answer :: [Char] -> Int
-- answer inputString = sum (priority `fmap` (sameInTwoElem (firstHalf inputString) (secondHalf inputString)))

testString = ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"] :: [[Char]]
type Parser = Parsec Void Text
singleLetterP :: Parser Char
singleLetterP = char 'h'

fullParser :: Parser [Int]
fullParser = many pUri
pScheme :: Parser Text
pScheme = choice
  [ string "data"
  , string "file"
  , string "ftp"
  , string "http"
  , string "https"
  , string "irc"
  , string "mailto" ]

pScheme' :: Parser Text
pScheme' = choice
  [ string "A" 
  , string "B"
  , string "C" ]

pMyChoice :: Parser Text
pMyChoice = choice
  [ string "X" 
  , string "Y"
  , string "Z" ]

data Uri = Uri
  { uriScheme :: Text
  } deriving (Eq, Show)

pUri :: Parser Int 
pUri = do
  theirChoice <- pScheme'
  s <- hspace
  myChoice <- pMyChoice
  newl <- newline

  return (points' theirChoice myChoice)

points :: Text -> Text -> Int
points "A" "X" = 1 + 3
points "A" "Y" = 2 + 6
points "A" "Z" = 3 + 0
points "B" "X" = 1 + 0
points "B" "Y" = 2 + 3
points "B" "Z" = 3 + 6
points "C" "X" = 1 + 6
points "C" "Y" = 2 + 0
points "C" "Z" = 3 + 3

points' :: Text -> Text -> Int
points' "A" "X" = 3 + 0
points' "A" "Y" = 1 + 3
points' "A" "Z" = 2 + 6
points' "B" "X" = 1 + 0
points' "B" "Y" = 2 + 3
points' "B" "Z" = 3 + 6
points' "C" "X" = 2 + 0
points' "C" "Y" = 3 + 3
points' "C" "Z" = 1 + 6
