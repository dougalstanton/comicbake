module Layout where

import Data.Maybe (mapMaybe)

import Script

type Dim = (Int,Int)
type Pt = Dim
type Order = [Int]


x1,y1,x2,y2 :: Frame -> Int
x1 = fst . head
y1 = snd . head
x2 = x1 . drop 1
y2 = y1 . drop 1
midx f = x1 f + ((x2 f + x1 f)`div`2)
midy f = y1 f + ((y2 f + y1 f)`div`2)


-- A single comic panel, with speech bubbles,
-- character outlines, etc.

data Panel = Panel { number     :: Int
		   , background :: FilePath
		   , characters :: [Frame]
		   , bubbles    :: [Frame]
		   } deriving Show

strSize :: [String] -> Dim
strSize str = ((5*) $ maximum $ map length str, 10*length str)

oddsweep = reverse [5..11]
evensweep = [1..7]


inFrames :: [Frame] -> Pt -> Bool
inFrames fs pt = any (inLine pt) fs
  where inFrame d f = fst d < x2 f && fst d > x1 f || snd d < y2 f && snd d > y1 f
	-- Is this point in the same row as a frame?
        inLine p f = snd p < y2 f && snd p > y1 f

allPos :: Dim -> Bool
allPos d = fst d > 0 && snd d > 0

--
-- Convert a scene into a panel
--

f :: Scene -> Panel
f s = foldr enplace base (sceneAction s)
 where base = Panel { number = sceneNumber s
		    , background = sceneBackground s
		    , characters = mapMaybe position $ sceneAction s
		    , bubbles = [] }

genlocs :: Dim -> Frame -> [Dim]
genlocs d f = [(x,y)| x <- [(x1 f - fst d)..(x2 f + fst d)], y <- [(y1 f - snd d)..(y2 f + snd d)]]

getlocation :: Action -> [Frame] -> Frame
getlocation a fs = [d', (fst d + fst d', snd d + snd d')]
	where d' = head $ filter allPos $ filter (not . inFrames fs) $ genlocs d (maybe origin id $ position a)
              d = strSize $ speech a
	      origin = [(0,0),(0,0)]

-- Decide best place to put this speech bubble in the
-- provided panel, then place it there.
enplace :: Action -> Panel -> Panel
enplace a p = p { bubbles = loc : bubbles p }
 where loc = getlocation a (characters p ++ bubbles p)

