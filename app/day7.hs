module Main where
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as Map
import Data.Void
import Data.Char (isPrint)
import Data.Text (Text, splitOn, intercalate)
import Data.List (foldl')

main :: IO ()
main = do
    inputText <- TIO.readFile "input/day7_reduced.txt"
    case parse pInput "" inputText of
      Left e ->  print (errorBundlePretty e) --print ("parse error" ++ show e)
      Right y -> print $ reverse $ Map.toList $ snd (foldl' itemToMap (Path "", Map.empty) y)

type Parser = Parsec Void Text

data StructuredInput = ChangeDir Text
                     | LsDir
                     | Dir Text
                     | File Integer Text
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
--pChangeDir = ChangeDir <$> ("$ cd " *> many printChar)
pChangeDir = do
  _   <- Text.Megaparsec.chunk "$ cd "
  dir <- takeWhile1P (Just "print character") isPrint
  return (ChangeDir dir)

pLsDir :: Parser StructuredInput
pLsDir = LsDir <$ "$ ls"

pDir :: Parser StructuredInput
--pDir = Dir <$> ("dir " *> many letterChar)
pDir = do
  _ <- Text.Megaparsec.chunk "dir "
  dir <- takeWhile1P (Just "print char") isPrint
  return (Dir dir)

pFile :: Parser StructuredInput
--pFile = File <$> (pInteger) <*> (many (letterChar <|> punctuationChar))-- <* many printChar) -- <*> (many letterChar)
pFile = do
  size <- pInteger
  fname <- takeWhile1P (Just "p char") isPrint
  return (File size fname)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

newtype Path = Path Text deriving (Eq,Ord,Show)
newtype DirOrFileName = DirOrFileName Text deriving (Eq,Ord,Show)
newtype Size = Size Integer deriving (Eq, Ord, Show)

type InputMap = Map.Map Path [(DirOrFileName, Size)]

fChangeDir :: Path -> Text -> Path
fChangeDir (Path p) ".." = Path $ "/" <> intercalate "/" (drop 2 $ splitOn "/" p)
fChangeDir (Path _) "/"    = Path "/"
fChangeDir (Path p) s    = Path $ "/" <> s <> p

addDirToValue :: Path -> InputMap -> DirOrFileName -> Size -> InputMap
-- need to check if value is in Map already or not. Insert if not
addDirToValue path m dir size
  | Map.member path m == True = Map.adjust ([(dir, size)] <>) path m
  | Map.member path m == False = Map.insert path [(dir, size)] m
  | otherwise = m

itemToMap :: (Path, InputMap) -> StructuredInput -> (Path, InputMap)
itemToMap (Path s, answerMap) input =
  case input of
    (ChangeDir dir)   -> (fChangeDir (Path s) dir, answerMap)
    LsDir             -> (Path s, answerMap)
    (Dir dir)         -> (Path s, addDirToValue (Path s) answerMap (DirOrFileName $ dir <> s) (Size 0))
    (File size name)  -> (Path s, addDirToValue (Path s) answerMap (DirOrFileName $ name <> s) (Size size))

type SizeMap = Map.Map Path Size

-- do another foldl' here to get a final map
sizeOfDirectory :: (Path, [(DirOrFileName, Size)]) -> SizeMap
sizeOfDirectory = undefined
  -- where list = Map.reverse $ Map.toList inputMap
