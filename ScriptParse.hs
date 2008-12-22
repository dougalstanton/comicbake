module ScriptParse where

import Control.Monad (liftM, liftM2, liftM3)
import Text.ParserCombinators.Parsec hiding (space, spaces)
import qualified Text.ParserCombinators.Parsec as P
import Data.List.Utils (split)
import Data.List (stripPrefix, isPrefixOf)
import Data.Maybe (mapMaybe)

import Script

type ScriptParser a = GenParser Char () a

rawScript :: String -> Script [RawScript]
rawScript str = Script { scriptTitle = title
                       , scriptCredits = credits
                       , scriptContents = contents }
    where (headerblock,contents) = span ("-- "`isPrefixOf`) $ preprocess str
          (title:credits) = case lines (head headerblock) of
                                [] -> ["",""]
                                ls -> mapMaybe (stripPrefix "-- ") ls

preprocess :: String -> [String]
preprocess = filter (not . null) . split "Scene "

scene :: ScriptParser (String,Maybe String,[(String,[String])])
scene = liftM3 (,,) sceneheader (eols >> scenedescription) speeches

sceneheader :: ScriptParser String
sceneheader = do    many1 digit <?> "scene number"
                    char '.' >> spaces
                    filename

filename :: ScriptParser FilePath
filename = between (char '[') (char ']') (many (noneOf "]\n"))
            <?> "image filename"

scenedescription :: ScriptParser (Maybe String)
scenedescription = do (try (char '(') >>
                            Just `liftM` manyTill (noneOf ")") (char ')'))
                        <|> return Nothing

speeches = eols >> (many speech <?> "speeches")

speech = liftM2 (,) speaker (newline >> spoken) <?> "speech"

speaker :: ScriptParser String
speaker = noneOf ":\n" `manyTill` char ':'
            <?> "character name"

spoken :: ScriptParser [String]
spoken = (indent >> many (noneOf "\n")) `sepEndBy1` (newline)
            <?> "dialogue"

indent = many1 (space <|> tab) <?> "indent"
space = char ' '
spaces = many1 space
eols = many newline
