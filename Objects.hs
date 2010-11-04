module Objects where

import Data.List

import Script
import Parse

-- Collect all the individual objects which are mentioned in the
-- script but don't assign them any location yet. They are like
-- cut-out bits of paper: we glue them all together in the next
-- stage.

data Text = Bubble [String] Character deriving (Eq,Show)
data Person = Person {unperson :: Character} deriving (Eq,Show)
data Objects = Objs [Text] [Person] deriving (Eq,Show)

action2text :: Action -> Text
action2text a = Bubble (speech a) (character a)

allcharacters :: [Action] -> [Character]
allcharacters = nub . map character

gatherobjects :: [Action] -> Objects
gatherobjects input = Objs dialogue people
  where people = map Person $ allcharacters input
        dialogue = map action2text input
