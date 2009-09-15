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

data Script a = Script
    { scriptTitle    :: String
    , scriptCredits  :: [String]
    , scriptLocation :: FilePath
    , scriptContents :: a
    } deriving (Eq, Show)

instance Functor Script where
    fmap f script = script { scriptContents = f (scriptContents script) }

