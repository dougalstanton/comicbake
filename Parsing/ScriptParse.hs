module Parsing.ScriptParse
       (Scene (..), Action (..)
       ,parseScript, parseScriptFromFile) where

import Text.ParserCombinators.Parsec hiding (space, spaces)
import Text.ParserCombinators.Parsec.Error
import Data.Maybe
import Data.List.Split (split, dropInitBlank, keepDelimsL, whenElt)
import Data.List (mapAccumL)

import Script hiding (Panel(..))
import qualified Script as S

data Scene = Scene
    { sceneNumber     :: Int
    , sceneBackground :: FilePath
    , sceneAction     :: [Action]
    } deriving (Eq, Show)

data Action = Action
    { character :: Character
    , speech    :: [String]
    } deriving (Eq, Show)

parseScriptFromFile fp = parseScript fp `fmap` readFile fp

parseScript :: FilePath -> String -> Either String (Script [S.Panel [Action]])
parseScript fp str = if null errs
                     then Right $ convert fp $ fromOk res
                     else Left $ unlines errs
  where res = runParser document [] fp str
        -- create proper error messages
        errs = either (map messageString . errorMessages) problems res
        fromOk (Right a) = a

sanitiseScene :: Scene -> S.Panel [Action]
sanitiseScene sc = S.Panel
    { S.number = sceneNumber sc
    , S.background = sceneBackground sc
    , S.bgsize = Nothing
    , S.action = sceneAction sc }

convert :: FilePath -> [S] -> Script [S.Panel [Action]]
convert fp ss = Script t [a,d] fp (map (sanitiseScene . mkScene) scenes)
 where (Preamble t s d a ms) = head ss
       scenes = splitRun isStart $ tail ss
       splitRun p = split (dropInitBlank $ keepDelimsL $ whenElt p)

mkScene :: [S] -> Scene
mkScene = foldr f z
  where z = Scene 0 "" []
        f (Start n url) s = s { sceneNumber = n, sceneBackground = url }
        f (Content c d) s = s { sceneAction = newAction c d s }
        newAction c d s@(Scene _ _ a) = Action c d : a

problems :: [S] -> [String]
problems ss = mapMaybe ($ss) ps
  where ps = [onePreamble,allBackgrounds]

onePreamble :: [S] -> Maybe String
onePreamble ss = if ps then Nothing else Just errmsg
  where ps = all isPreamble $ take 1 ss
        errmsg = "The beginning of the script must contain a preamble"

allBackgrounds :: [S] -> Maybe String
allBackgrounds ss | length s < 1  = Just "You need at least one scene."
                  | noBg (head s) = Just "First scene needs a bg image."
		  | otherwise     = Nothing
  where s = filter isStart ss
	noBg (Start _ bg) = null bg

isPreamble (Preamble _ _ _ _ _) = True
isPreamble _                    = False
isStart    (Start _ _)          = True
isStart    _                    = False
isContent  (Content _ _)        = True
isContent  _                    = False

-- We use this type as a temporary measure for parsing
-- the data in the script, before sanity checking.
data S = Preamble String String String String [String]
       | Start Int String
       | Content String [String]
	deriving (Eq, Show)

--
-- Below here is the base level parser using Parsec.
--

type ScriptParser a = GenParser Char FilePath a

space = char ' '
spaces = many1 space
newlines = many newline
newlines1 = many1 newline

document :: ScriptParser [S]
document = do p <- preamble <?> "preamble"
	      s <- script
              return (p:s)

-- decide on data requirements and use count or permute function
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

-- If no filename is given we fill in the most recently
-- used one. Then we store the current filename for later.
sceneheader = do try scenekw ; spaces
                 num <- many1 digit ; char '.' ; many space
		 url <- optfile
                 setState url
		 newlines
		 return $ Start (read num) url
  where optfile = do oldurl <- getState
                     if null oldurl
                        then filename <?> "file path"
                        else option oldurl filename

-- First character of name must be a letter
action = do name <- realname ; char ':'
	    speech <- many (spaces >> legalchar `manyTill` newline)
	    newlines
            return $ Content name speech
  where legalchar = noneOf "\n\r"
        realname = do c <- letter <?> "character name"
                      cs <- many (letter <|> space)
                      return (c:cs)
