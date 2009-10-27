module Layout (Panel(..), Bubble(..), scene2panel) where

import Control.Arrow ((***))

import Data.Maybe (mapMaybe,isJust)
import Data.Ix (inRange)
import Data.List
import Data.Ord (comparing)

import Script (IsFrame(..), Box(..), Frame, Dim, Pt)
import Parse (Scene(..), Action(..))
import SA

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
frame2box _ = error "Layout: Non-square areas not supported"

-- A speech bubble, a type of Frame
data Bubble = Bubble { content :: [String]
                     , area    :: Box
		     , anchor  :: Box
		     , size    :: Dim
		     , seqnum  :: Int
		     } deriving (Eq,Show)

instance IsFrame Bubble where
 dim	= size
 coords	= coords . area

-- A speech bubble that doesn't have an assigned
-- location, but floats freely...
data FloatingBubble = FBubble { floatingspeech :: [String]
                              , dimension      :: Dim
			      , characterloc   :: Box
			      , bubbleorder    :: Int
			      } deriving (Eq,Show)

instance IsFrame FloatingBubble where
 dim    = dimension
 coords = coords . characterloc

-- A single comic panel, with speech bubbles,
-- character outlines, etc.

data Panel = Panel { number     :: Int
		   , background :: (FilePath,Dim)
		   , characters :: [Box]
		   , bubbles    :: [Bubble]
		   , lowpt      :: Pt
		   } deriving Show

instance IsFrame Panel where
  dim = snd . background
  coords = error "Panel has no co-ordinates"

--
-- Convert a scene into a panel
--

scene2panel:: Dim -> [Dim] -> [Double] -> Scene -> Panel
scene2panel bgsize txtsizes rands s = base {bubbles = fromGrid result}
 where base = Panel { number = sceneNumber s
		    , background = (sceneBackground s, bgsize)
		    , characters = map frame2box $ mapMaybe position $ sceneAction s
		    , bubbles = []
		    , lowpt = (0,0) }
       floating = zipWith3 action2fbubble [1..] txtsizes (sceneAction s)
       sacfg = Config { temperature=50
                      , coolingfactor=0.95
		      , randoms = rands
		      }
       sastate = State { state = (initialGrid base floating)
                       , value = cost
		       , newstate = perturbGrid
		       }
       result = anneal sacfg sastate

action2fbubble :: Int -> Dim -> Action -> FloatingBubble
action2fbubble i sz a =
  FBubble { floatingspeech = speech a
          , dimension = sz
	  , characterloc = maybe nullbox frame2box (position a)
	  , bubbleorder = i
	  }

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

-- Mapping from co-ordinate to a sequence number.
-- Use Nothing to represent an empty square.
type Grid = [(Pt,Maybe FloatingBubble)]
type SmallGrid = [(Pt,FloatingBubble)]
type Env  = [Box]

keepGood :: Grid -> SmallGrid
keepGood = concatMap f
  where f (_,Nothing) = []
        f (a,Just fb) = [(a,fb)]

-- Generate a grid of candidate locations, omitting
-- any locations which overlap the known characters.
blankGrid :: Panel -> Dim -> Grid
blankGrid p (mw,mh) = [((cx,cy),Nothing)
                      | cy <- cys, cx <- cxs, valid (cx,cy)]
  where (w,h) = dim p
        cxs = [mw,mw+mw..w]
	cys = [mh,mh+mh..h]
	valid pt = overlapping (characters p) (Box pt pt)

fromGrid :: (Env,Grid) -> [Bubble]
fromGrid (_,g) = map mkbubble $ keepGood g
  where mkbubble (pt,fb) = Bubble { content = floatingspeech fb
		                  , anchor  = characterloc fb
				  , size    = dim fb
				  , area    = conv pt (dim fb)
				  , seqnum  = bubbleorder fb
				  }
	conv (mx,my) (w,h) = Box (mx,my) (mx+w,my+h)

-- Add all the bubbles spread evenly across the panel
-- in whatever order they arrive.
initialGrid :: Panel -> [FloatingBubble] -> (Env,Grid)
initialGrid p fbs = (e,g)
  where minDims = (minimum *** minimum) $ unzip $ map dim fbs
	grid = blankGrid p minDims
        n = length fbs
	m = length grid
	incr = m`div`n
	bubs = intercalate (replicate incr Nothing)
	                       (map (return . Just) fbs)
	g = zipWith (\a (b,_) -> (b,a)) bubs grid
	e = characters p

-- Determine some cost for this generated layout.
cost :: (Env,Grid) -> Double
cost (e,g) = disorderCost g' + overlapCost e g' + attributionCost e g'
  where g' = keepGood g

-- Penalise solutions where the bubbles appear
-- out of order for the x,y co-ordinates.
disorderCost :: SmallGrid -> Double
disorderCost g = sum $ zipWith diff [1..] sorted
  where sorted = map (bubbleorder . snd) $ sortBy (comparing fst) g

        diff :: Int -> Int -> Double
	diff a b = fromIntegral $ abs (a - b)

-- Penalise solutions where bubbles overlap each
-- other or the characters' faces.
overlapCost :: Env -> SmallGrid -> Double
overlapCost e g = fromIntegral $ overlapfaces + overlapother
  where mkbox :: (Pt,FloatingBubble) -> Box
        mkbox ((px,py),fb) = let (w,h) = dim fb in Box (px,py) (px+w,py+h)

	boxes = map mkbox g

        -- remove boxes which duplicate others
	nodups = nubBy overlaps boxes
	overlapother = 2 * (length boxes - length nodups)

	-- remove boxes which overlap faces
	freespace = filter (overlapping e) boxes
	overlapfaces = (length boxes - length freespace)

-- Penalise solutions where bubbles are further
-- from the anchor than they are from another
-- anchor point.
attributionCost :: Env -> SmallGrid -> Double
attributionCost e g = 0.5 * fromIntegral (length $ filter incorrect g)
  where incorrect pfb = nearest pfb /= characterloc (snd pfb)

        nearest :: (Pt,FloatingBubble) -> Box
        nearest (pt,_) = minimumBy (comparing (dist pt)) e

dist :: (IsFrame a) => Pt -> a -> Double
dist (mx1,my1) pt2 = sqrt $ fromIntegral (xd*xd + yd*yd)
  where (mx2,my2) = midpoint pt2
	xd = (mx2 - mx1)
	yd = (my2 - my1)

-- Zip together two lists using 'f', but when
-- the shorter of the two lists ends we transform
-- the tail of the longer list using 'g' and tag
-- it on the end.
longZipWith :: (a -> a -> b) -> (a -> b) -> [a] -> [a] -> [b]
longZipWith _ g [] bs = map g bs
longZipWith _ g as [] = map g as
longZipWith f g (a:as) (b:bs) = f a b : longZipWith f g as bs

-- Take proportion r of the bubbles and swap their
-- locations with the same number of empty places.
perturbGrid :: Double -> (Env,Grid) -> (Env,Grid)
perturbGrid r (e,g) = (e,real2 ++ empty2 ++ perturbed)
  where (real,empty) = partition (isJust . snd) g
        n = round $ r * genericLength real
	(real1,real2) = splitAt n real
	(empty1,empty2) = splitAt n empty
        perturbed = concat $ longZipWith swap return real1 empty1
	swap (pt1,n1) (pt2,n2) = [(pt1,n2),(pt2,n1)]
