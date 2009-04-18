module Parse where

import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Text.ParserCombinators.Parsec hiding (space, spaces)
import qualified Text.ParserCombinators.Parsec as P
import Data.List.Utils (split)
import Data.List (stripPrefix, isPrefixOf)
import Data.Maybe (mapMaybe)

import Script

data S = Preamble String String String String [String]
       | Start Int (Maybe String)
       | Content String [String]
	deriving (Show)
type ScriptParser a = GenParser Char () a

space = char ' '
spaces = many1 space
newlines = many newline
newlines1 = many1 newline

document = do p <- preamble
	      s <- script
              return (p:s)

-- decide on data requirements and use chount or permute function
preamble = do title  <- commentline
	      series <- commentline
              date   <- commentline
              author <- commentline
	      misc   <- many (commentline)
	      newlines
  	      return $ Preamble title series date author misc
              
 where commentline = string "--" >> spaces >> manyTill anyChar newlines1

scenekw = string "Scene" <?> "scene header"

script = manyTill (sceneheader <|> action) eof

-- todo: find a proper legal filename characters
filename = between (char '<') (char '>') legalchars
  where legalchars = many (alphaNum <|> char '.' <|> char '/')

sceneheader = do try scenekw ; spaces
                 num <- many1 digit ; char '.' ; many space
		 url <- optionMaybe filename
		 newlines
		 return $ Start (read num) url

-- First character of name must be a letter
action = do name <- realname ; char ':'
	    speech <- many (spaces >> legalchar `manyTill` newline)
	    newlines
            return $ Content name speech
  where legalchar = noneOf "\n\r"
        realname = do c <- letter <?> "character name"
                      cs <- many (letter <|> space)
                      return (c:cs)
