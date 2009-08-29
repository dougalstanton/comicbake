module Layout where

import Data.Maybe (mapMaybe)

import Script

type Dim = (Int,Int)
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
		   }

strSize :: [String] -> Dim
strSize str = ((5*) $ maximum $ map length str, 10*length str)

oddsweep = reverse [5..11]
evensweep = [1..7]

--
-- Convert a scene into a panel
--

f :: Scene -> Panel
f s = foldr enplace base (sceneAction s)
 where base = Panel { number = sceneNumber s
		    , background = sceneBackground s
		    , characters = mapMaybe position $ sceneAction s
		    , bubbles = [] }

genlocs :: Dim -> Frame -> [Frame]
genlocs d f = (midx f, midy f)

getlocation :: Dim -> Action -> [Frame] -> Frame
getlocation d a fs = head $ filter fs $ genlocs d (position a)

enplace :: Action -> Panel -> Panel
enplace a p = p { bubbles = loc : bubbles p }
 where loc = getlocation a (characters p ++ bubbles p)



