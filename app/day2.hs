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

main :: IO ()

-- main = getArgs >>= parse >>= putStr . tac
main = do
    args <- getArgs
    inputString <- Data.Text.IO.readFile "input/day2.txt"
    case parse fullParser "" inputString  of
        Left e -> print ("parse error" ++ show e)
        Right y -> print (sum y)
    -- sum p
    --p <- parseTest (many (pUri) :: Parser [Int]) inputString
    --sum p

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
