module Parsing.ScriptParse
       (Action (..)
       ,parseScript, parseScriptFromFile) where

import Control.Arrow (left)
import Control.Monad (mplus)
import Text.ParserCombinators.Parsec
import Data.Maybe
import Data.Char (isSpace)

import Script

data Action = Action
    { character :: Character
    , thinking  :: Bool
    , speech    :: [String]
    } deriving (Eq, Show)

data ComicState = S { savedUrl :: Maybe FilePath }
emptyState = S Nothing
saveUrl url = updateState (\s -> s {savedUrl = url})

ifIndented yes no = do pos <- getPosition
                       if (sourceColumn pos > 1) then yes else no

type ScriptParser a = GenParser Char ComicState a

parseScriptFromFile :: FilePath -> IO (Either String (Script [Panel [Action]]))
parseScriptFromFile fp = parseScript fp `fmap` readFile fp

parseScript :: FilePath -> String -> Either String (Script [Panel [Action]])
parseScript fp str = left show $ runParser scriptParser emptyState fp str

--
-- Below here is the base level parser using Parsec.
--


scriptParser :: ScriptParser (Script [Panel [Action]])
scriptParser = do bare   <- preambleParser
                  scenes <- manyTill sceneParser eof
                  return $ bare { scriptContents = scenes }

preambleParser :: ScriptParser (Script [Panel [Action]])
preambleParser = do metadata <- catMaybes `fmap` many comment
                    let title  = fetch "title" metadata
                        author = fetch "author" metadata
                    file <- (sourceName . statePos) `fmap` getParserState
                    many (space <|> newline)
                    return $ Script title [author] file []
    where fetch key = fromMaybe ("Unknown "++key) . lookup key

sceneParser :: ScriptParser (Panel [Action])
sceneParser = do (num,bgfile) <- sceneHeaderParser
                 speeches     <- speechListParser
                 return $ Panel num bgfile Nothing speeches

sceneHeaderParser :: ScriptParser (Int,FilePath)
sceneHeaderParser = do sceneKeyword >> spaces
                       num <- many1 digit <?> "scene number"
                       char '.' >> spaces
                       url <- urlParser <?> "path to background image"
                       many (space <|> newline)
                       return (read num, url)
    where urlParser = do oldUrl <- savedUrl `fmap` getState
                         newUrl <- optionMaybe filename
                         let url = newUrl `mplus` oldUrl
                         maybe pzero (\u -> saveUrl url >> return u) url

sceneKeyword :: ScriptParser String
sceneKeyword = string "Scene" <?> "Scene header"

speechListParser :: ScriptParser [Action]
speechListParser = do { eof; return [] }
                   <|> do { lookAhead sceneKeyword ; return [] }
                   <|> do speech <- speechParser
                          others <- speechListParser
                          return (speech:others)

speechParser :: ScriptParser Action
speechParser = do speaker <- speakerParser <?> "speaker's name"
                  thought <- try modeParser <?> "optional thought marker"
                  speech  <- textParser <?> "speech"
                  return $ Action speaker thought speech

speakerParser :: ScriptParser String
speakerParser = do name <- manyTill anyChar (char ':')
                   spaces
                   return (trim name)
  where trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

modeParser :: ScriptParser Bool
modeParser = do mode <- optionMaybe (string "(Thinking)" >> spaces >> newline)
                return (isJust mode)

textParser :: ScriptParser [String]
textParser = spaces >> ifIndented lineParser (return [])
  where lineParser = do l  <- anyChar `manyTill` newline
                        ls <- textParser
                        return (l:ls)

filename :: ScriptParser FilePath
filename = between (char '<') (char '>') (many (noneOf "\n\r>"))

comment :: ScriptParser (Maybe (String,String))
comment = do delimiter
             mkv <- optionMaybe (try keyvalue)
             optional (manyTill anyChar newline)
             many newline
             return mkv
  where delimiter = string "--" >> spaces

keyvalue :: ScriptParser (String,String)
keyvalue = do key <- many1 letter
              char ':' >> spaces
              val <- manyTill anyChar newline
              return (key,val)
