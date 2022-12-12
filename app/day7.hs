module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

main :: IO ()
main = do
    inputString <- TIO.readFile "input/day7.txt"
    case parse pInput "" inputString of
      Left e ->  print (errorBundlePretty e) --print ("parse error" ++ show e)
      Right y -> print y

type Parser = Parsec Void Text

data StructuredInput = ChangeDir String
                     | LsDir
                     | Dir String
                     | File Integer -- also has a name, but we dont need it
                     deriving Show

pInteger :: Parser Integer
pInteger = lexeme L.decimal

pInput :: Parser [StructuredInput]
pInput = many (pLine <* newline)

pLine :: Parser StructuredInput
pLine =
  pChangeDir <|>
  pLsDir <|>
  pDir <|>
  pFile

pChangeDir :: Parser StructuredInput
pChangeDir = ChangeDir <$> ("$ cd " *> many printChar)

pLsDir :: Parser StructuredInput
pLsDir = LsDir <$ "$ ls"

pDir :: Parser StructuredInput
pDir = Dir <$> ("dir " *> many letterChar)

pFile :: Parser StructuredInput
pFile = File <$> (pInteger <* many printChar) -- <*> (many letterChar)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
