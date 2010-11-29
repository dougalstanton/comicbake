module Script (Frame, Pt, Dim, Character,
               IsFrame (..),
               Box (..),
               Script (..),
               Panel (..)) where

import Control.Arrow ((***))

type Character = String
type Dim       = (Int,Int)
type Pt        = (Int,Int)
type Frame     = [Pt] -- box containing character

class IsFrame a where
 dim		:: a -> Dim
 coords		:: a -> [Pt]
 loc		:: a -> Pt
 midpoint	:: a -> Pt
 -- default implementations
 loc = head . coords
 coords = return . loc
 midpoint = (avg *** avg) . unzip . coords
   where avg xys = sum xys `div` length xys

data Box = Box { topleft	:: Pt
               , bottomright	:: Pt
	       } deriving (Eq, Show)
instance IsFrame Box where
 coords b = [topleft b, bottomright b]
 dim b = let (x1,y1) = topleft b
             (x2,y2) = bottomright b
	 in (x2-x1, y2-y1)
instance Ord Box where
 compare (Box (ax1,ay1) (ax2,ay2))
         (Box (bx1,by1) (bx2,by2))
            | ay2 < by1 = LT
            | ay1 > by2 = GT
            -- | ax1 > bx2 = compare ay1 by1
            -- | ax2 < bx1 = compare ay2 by2
            | otherwise = EQ

-- The grand-daddy of the containers in this program.
-- Everything is stored in the script once it's parsed,
-- and transformations are done on the contents until the
-- final panels are boiled down to one image and written.
data Script a = Script
    { scriptTitle    :: String
    , scriptCredits  :: [String]
    , scriptLocation :: FilePath
    , scriptContents :: a
    } deriving (Eq, Show)

instance Functor Script where
    fmap f script = script { scriptContents = f (scriptContents script) }

-- A script contains a number of panels representing the
-- "scenes", which carry their own metadata as well as the
-- current state of the panels contents.
data Panel a = Panel
    { number     :: Int
    , background :: FilePath
    , bgsize     :: Maybe Dim
    , action     :: a
    } deriving Show

instance Functor Panel where
 fmap f p = p { action = f (action p) }
