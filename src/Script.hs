{-# LANGUAGE DeriveFunctor #-}
module Script where

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

-- Metadata which we use when writing the comic header/footer
-- and when uploading. Some of these can be empty but it doesn't
-- really matter if they are, so option types are unnecessary.
data Info = Info
    { title       :: String
    , author      :: String
    , date        :: String
    , description :: String
    , credits     :: [String]
    , tags        :: [String]
    } deriving (Eq,Show)
-- The grand-daddy of the containers in this program.
-- Everything is stored in the script once it's parsed,
-- and transformations are done on the contents until the
-- final panels are boiled down to one image and written.
data Script a = Script
    { scriptInfo     :: Info
    , scriptLocation :: FilePath
    , scriptContents :: a
    } deriving (Eq, Show, Functor)

-- A script contains a number of panels representing the
-- "scenes", which carry their own metadata as well as the
-- current state of the panels contents.
data Panel a = Panel
    { number     :: Int
    , background :: FilePath
    , bgsize     :: Maybe Dim
    , action     :: a
    } deriving (Show, Functor)
