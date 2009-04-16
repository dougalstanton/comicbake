module Parse where

import Control.Monad (liftM, liftM2, liftM3, liftM4)
import Text.ParserCombinators.Parsec hiding (space, spaces)
import qualified Text.ParserCombinators.Parsec as P
import Data.List.Utils (split)
import Data.List (stripPrefix, isPrefixOf)
import Data.Maybe (mapMaybe)

import Script

data S = Start Int (Maybe String) | Content String [String]
	deriving (Show)
type ScriptParser a = GenParser Char () a

space = char ' '
spaces = many1 space
newlines = many newline

scenekw = string "Scene"

-- todo: doesn't handle eof properly
script = do s <- many (newlines >> (sceneheader <|> action))
	    eof
	    return s


filename = between (char '<') (char '>') legalchars
  where legalchars = many (letter <|> char '.' <|> char '/') -- todo: fix

sceneheader = do try scenekw ; spaces
                 num <- many1 digit ; char '.' ; many space
		 url <- optionMaybe filename
		 return $ Start (read num) url

-- todo: first character of name should not be space
action = do name <- many1 (letter <|> space) ; char ':'
	    speech <- many (spaces >> legalchar `manyTill` newline)
            return $ Content name speech
  where legalchar = noneOf "\n\r"
