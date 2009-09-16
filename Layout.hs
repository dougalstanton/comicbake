module Layout (Panel(..), Bubble(..), scene2panel) where

import Debug.Trace
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (intersect)

import Script (Script(..), IsFrame(..), Box(..), Frame, Dim, Pt)
import Parse (Scene(..), Action(..))


x1,y1,x2,y2 :: Frame -> Int
x1 = fst . head
y1 = snd . head
x2 = x1 . drop 1
y2 = y1 . drop 1


-- A speech bubble, a type of Frame
data Bubble = Bubble { content :: [String]
                     , area    :: Frame
		     , anchor  :: Frame
		     , fakesize:: Dim
		     } deriving (Eq,Show)

instance IsFrame Bubble where
 dim	= fakesize
 coords	= area

-- A single comic panel, with speech bubbles,
-- character outlines, etc.

data Panel = Panel { number     :: Int
		   , background :: FilePath
		   , characters :: [Frame]
		   , bubbles    :: [Bubble]
		   , lowpt      :: Pt
		   } deriving Show

-- this is total guesswork, which gives wrong
-- results most of the time. awesome!
strSize :: [String] -> Dim
strSize str = (width, height)
  where height = 30 + (15*length str) -- 12pt + leading + tail
        width = (7*) $ maximum $ map length str

--
-- Convert a scene into a panel
--

scene2panel:: Scene -> Panel
scene2panel s = foldl stick base (sceneAction s)
 where base = Panel { number = sceneNumber s
		    , background = sceneBackground s
		    , characters = mapMaybe position $ sceneAction s
		    , bubbles = []
		    , lowpt = (0,0) }

-- we're missing test for fr2 inside fr1
overlaps :: Frame -> Frame -> Bool
overlaps fr1 fr2 = a || b
 where a = overlapsV fr1 fr2 && overlapsH fr1 fr2
       b = overlapsV fr2 fr1 && overlapsH fr2 fr1

overlapsH :: Frame -> Frame -> Bool
overlapsH fr1 fr2 = a || b
 where a = between (x2 fr1) (x1 fr2) (x2 fr2) -- left/right
       b = between (x1 fr1) (x1 fr2) (x2 fr2) -- right/left
       between l m n = m > l && m < n
overlapsV :: Frame -> Frame -> Bool
overlapsV fr1 fr2 = a || b
 where a = between (y2 fr1) (y1 fr2) (y2 fr2) -- bottom/top
       b = between (y1 fr1) (y1 fr2) (y2 fr2) -- top/bottom
       between l m n = m > l && m < n

invalid :: [Frame] -> Frame -> Bool
invalid curs new = any (not . overlaps new) curs

-- look for some candidate spaces around the given
-- frame, beneath the given point for a new frame
-- of the quoted dimensions. The area to look in
-- is given in the next argument.
candidates :: Frame -> Pt -> Dim -> Dim -> [Frame]
candidates fr (_,lowy) (w,h) (w',h') = pts
 where pts = [[(x,y),(x+w,y+h)] | y <- [lowy`max`(y1 fr - h')..y2 fr]
                                , x <- [0`max`(x1 fr - w')..x2 fr]]

-- repeatedly look for candidates in wider and wider
-- region around the character frame
search :: Frame -> Pt -> Dim -> [Frame]
search frame lowpt dim = concatMap (candidates frame lowpt dim) dims
 where multpair (x,y) m = (x*m,y*m)
       dims = map (multpair dim) [1..4]

stick :: Panel -> Action -> Panel
stick p a = p { bubbles = newbubble : bubbles p }
 where size = strSize $ speech a
       lowpt = if null (bubbles p) then (0,0)
                  else maximum $ map (head.tail.area) (bubbles p)
       charbox = fromMaybe [(0,0),(0,0)] (position a)
       cands = filter (invalid used) $ search charbox lowpt size
       used = map coords (bubbles p) ++ characters p
       newbubble = Bubble { content = speech a
                          , anchor = charbox
			  , fakesize = size
			  , area = head cands }

