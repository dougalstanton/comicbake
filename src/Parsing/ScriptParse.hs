module Parsing.ScriptParse
--     (Action (..)
--     ,parseScript, parseScriptFromFile)
       where

import Control.Applicative
import Control.Arrow (left)
import Control.Monad (mplus)
import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String
import Data.Maybe
import Data.Char (isSpace)

import Script

data Action = Action
    { character :: Character
    , thinking  :: Bool
    , speech    :: [String]
    } deriving (Eq, Show)

type ComicState = Maybe FilePath
saveUrl = updateState . const

ifIndented yes no = do pos <- getPosition
                       if (sourceColumn pos > 1) then yes else no

type ScriptParser a = GenParser Char ComicState a

parseScriptFromFile :: FilePath -> IO (Either String (Script [Panel [Action]]))
parseScriptFromFile fp = parseScript fp <$> readFile fp

parseScript :: FilePath -> String -> Either String (Script [Panel [Action]])
parseScript fp str = left show $ runParser scriptParser Nothing fp str

--
-- Below here is the base level parser using Parsec.
--

-- Applicative style doesn't look so good with infix operators
-- so define a couple of synonyms
pair :: a -> b -> (a,b)
pair a b = (,) a b
cons :: a -> [a] -> [a]
cons a b = a:b

-- Some utility parsers.
word, toEOL :: ScriptParser String
word =  many1 letter
toEOL = anyChar `manyTill` newline

scriptParser :: ScriptParser (Script [Panel [Action]])
scriptParser = Script <$> header <*> filepath <*> (scene `manyTill` eof)
  where filepath = sourceName . statePos <$> getParserState

header :: ScriptParser [(String,String)]
header = preamble <* many (space <|> newline)

scene :: ScriptParser (Panel [Action])
scene = Panel <$> index <*> background <*> pure Nothing <*> speechListParser
  where index = read <$> (string "Scene" *> spaces *> many1 digit <* char '.' <* spaces)
        background = backgroundfile <* spaces

backgroundfile :: ScriptParser FilePath
backgroundfile = do oldurl <- getState
                    newurl <- optionMaybe filename
                    let result = newurl `mplus` oldurl
                    case result of
                        Just url -> saveUrl result >> return url
                        Nothing  -> parserFail "path to background image"

speechListParser :: ScriptParser [Action]
speechListParser = ([] <$ eof <|> [] <$ lookAhead (string "Scene"))
                   <|> cons <$> speechParser <*> speechListParser

speechParser :: ScriptParser Action
speechParser = Action <$> speaker <*> is_thought <*> speech
  where speaker    = speakerParser <?> "speaker's name"
        is_thought = try modeParser <?> "optional thought marker"
        speech     = textParser <?> "speech"

speakerParser :: ScriptParser String
speakerParser = trim <$> manyTill anyChar (char ':') <* spaces
  where trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

modeParser :: ScriptParser Bool
modeParser = isJust <$> optionMaybe (string "(Thinking)" >> spaces >> newline)

textParser :: ScriptParser [String]
textParser = spaces >> ifIndented (cons <$> toEOL <*> textParser) (return [])

filename :: ScriptParser FilePath
filename = between (char '<') (char '>') (many (noneOf "\n\r>"))

preamble :: ScriptParser [(String,String)]
preamble = between delimiter delimiter (many keyval)
    where delimiter = string "--" *> spaces
          keyval = pair <$> (word <* char ':' <* spaces) <*> toEOL

