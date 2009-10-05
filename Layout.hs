module Layout (Panel(..), Bubble(..), scene2panel) where

import Data.Maybe (mapMaybe)
import Data.Ix (inRange)

import Script (IsFrame(..), Box(..), Frame, Dim, Pt)
import Parse (Scene(..), Action(..))

-- Some selection functions
x, y :: Pt -> Int
x = fst
y = snd
x1,y1,x2,y2 :: Box -> Int
x1 = x . topleft
y1 = y . topleft
x2 = x . bottomright
y2 = y . bottomright

nullbox :: Box
nullbox = Box (0,0) (0,0)
frame2box :: Frame -> Box
frame2box (pt1:pt2:_) = Box pt1 pt2
frame2box _ = nullbox -- maybe an error?

-- A speech bubble, a type of Frame
data Bubble = Bubble { content :: [String]
                     , area    :: Box
		     , anchor  :: Box
		     , size:: Dim
		     } deriving (Eq,Show)

instance IsFrame Bubble where
 dim	= size
 coords	= coords . area

-- A single comic panel, with speech bubbles,
-- character outlines, etc.

data Panel = Panel { number     :: Int
		   , background :: (FilePath,Dim)
		   , characters :: [Box]
		   , bubbles    :: [Bubble]
		   , lowpt      :: Pt
		   } deriving Show

--
-- Convert a scene into a panel
--

scene2panel:: Dim -> [Dim] -> Scene -> Panel
scene2panel bgsize txtsizes s = foldl stick base sizedAction
 where base = Panel { number = sceneNumber s
		    , background = (sceneBackground s, bgsize)
		    , characters = map frame2box $ mapMaybe position $ sceneAction s
		    , bubbles = []
		    , lowpt = (0,0) }
       sizedAction = zip txtsizes (sceneAction s)
-- Test if two boxes overlap.
overlaps :: Box -> Box -> Bool
overlaps box1 box2 = a || b
 where a = overlapsV box1 box2 && overlapsH box1 box2
       b = overlapsV box2 box1 && overlapsH box2 box1

overlapsH :: Box -> Box -> Bool
overlapsH box1 box2 = a || b
 where a = (x2 box1) `between` (x1 box2, x2 box2) -- left/right
       b = (x1 box1) `between` (x1 box2, x2 box2) -- right/left
       between = flip inRange
overlapsV :: Box -> Box -> Bool
overlapsV box1 box2 = a || b
 where a = (y2 box1) `between` (y1 box2, y2 box2) -- bottom/top
       b = (y1 box1) `between` (y1 box2, y2 box2) -- top/bottom
       between = flip inRange

-- Check that a candidate box doesn't overlap
-- any previously-used areas.
overlapping :: [Box] -> Box -> Bool
overlapping curs new = any (not . overlaps new) curs

-- look for some candidate spaces around the given
-- frame, beneath the given point for a new frame
-- of the quoted dimensions. The area to look in
-- is given in the next argument.
candidates :: Box -> Pt -> Dim -> Dim -> [Box]
candidates box (_,lowy) (w,h) (w',h') = pts
 where pts = [Box (x',y') (x'+w,y'+h)
             | y' <- [lowy`max`(y1 box - h')..y2 box]
             , x' <- [0`max`(x1 box - w')..x2 box]]

-- repeatedly look for candidates in wider and wider
-- region around the character frame
search :: Box -> Pt -> Dim -> [Box]
search box low wh = concatMap (candidates box low wh) searchareas
 where multpair pair m = (m * x pair,m * y pair)
       searchareas = map (multpair wh) [1..4]

-- Place a new speech bubble into the current
-- comic panel.
stick :: Panel -> (Dim,Action) -> Panel
stick p (sz,a) = p { bubbles = newbubble : bubbles p
                   , lowpt = bottomright (area newbubble)
	           }
 where cands = filter (overlapping used) $ search newchar (lowpt p) sz
       used = map area (bubbles p) ++ characters p
       newchar = maybe nullbox frame2box (position a)
       newbubble = Bubble { content = speech a
                          , anchor = newchar
			  , size = sz
			  , area = head cands }

