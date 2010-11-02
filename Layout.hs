module Layout where

import Control.Arrow (first,second, (***))

import Script
import Locations
import Geometry

-- Given a list of texts associated with some location, we need
-- to determine the size of the texts by some mechanism and pin
-- these texts to that location.
--
-- Since we will only be guessing text size we want some method
-- of choosing a location that isn't too constrained to avoid
-- unnecessary overlaps.
--
-- From the centre point of each speaker's box we will create
-- equally positioned "clock arms" on which to pin the text.
-- Text bubbles can slide along these arms until the given
-- constraints are satisfied:
--
-- 1. that no text overlap a speaker or other text.
-- 2. that all text appears in traditional comic-strip order.
-- 3. avoid crowding/empty space where possible.
--
-- Diagonal arms are nicer than horizontal/vertical arms,
-- and arms towards the midpoint of conversation are better
-- than arms away from the conversation.

-- Guess how much space a speech bubble will take up by counting
-- number of lines, and characters per line. Multiply the width by
-- somewhere between 0.4 and 0.5 because that seems to be the right
-- ratio from some experiments.
estimate str = (scale w,h)
  where w = 20 * (maximum $ map length str)
        h = 20 * length str
        scale = round . (* 0.4) . fromIntegral

-- Create radial arms made of candidate centre-points.
-- (0,0) is top left, x++ moves right, y++ moves down
horiz :: Bool -> Pt -> [Pt]
horiz left = iterate (first (if left then pred else succ))
vertic :: Bool -> Pt -> [Pt]
vertic up = iterate (second (if up then pred else succ))
-- Diagonals use the vertical component from one arm and
-- the horizontal component from the other arm.
joinarms arm1 arm2 pt = zipWith f (arm1 pt) (arm2 pt)
  where f (x,_) (_,y) = (x,y)

-- Only consider points that are within the bounds of the original image.
inbounds (w,h) (x,y) = x >= 0 && x <= w && y >= 0 && y <= h

-- Sometimes it looks nice if a bubble spills over the edge a bit but
-- we don't want it to disappear completely.
-- TODO: Fine-tune how much margin we want to allow.
onscreen :: Dim -> [String] -> Pt -> Bool
onscreen (w,h) txt pt = go (-20,-20) (w+20,h+20) (txt2box pt txt)
  where go (left,top) (right,bottom) (Box (xl,yt) (xr,yb)) =
           and $ [xl>=left,yt>=top,xr<=right,yb<=bottom]

-- Arms heading in the general direction of the centre of the
-- scene are probably aesthetically nicer (guesswork). Arms at
-- a diagonal are nicer than ones directly above/below/beside
-- the speaker (guesswork). We will omit arms facing directly
-- away from the conversation because they're never worth it
-- (more guesswork).
-- TODO: Change arm order to top-bottom for vertical component
-- of diagonals. Conversations should favour starting higher
-- and moving down later.
type Arm = [Pt]
rankarms :: [String] -> Dim -> Pt -> [Arm]
rankarms txt sz@(pw,ph) pt@(x,y) = map keep candidates
  where keep = filter (onscreen sz txt) . takeWhile (inbounds sz)
        hdir = x > (pw `div` 2) -- left?
        vdir = y > (ph `div` 2) -- up?
        diags = [ joinarms (horiz hdir)       (vertic vdir)
                , joinarms (horiz hdir)       (vertic (not vdir))
                , joinarms (horiz (not hdir)) (vertic vdir)]
        flats = [vertic vdir, horiz hdir, vertic (not vdir)]
        candidates = map ($pt) (diags ++ flats)

-- Process the text bubbles, testing each candidate location for
-- suitability and discarding the undesirable ones. There will
-- inevitably be tradeoffs where an ideal set of positions is not
-- easy to determine. Removing poorer choices incrementally might
-- help to clarify the issue.

txt2box (x,y) txt = Box (x-w, y-h) (x+w, y+h)
  where (w,h) = half *** half $ estimate txt
        half = flip div 2

-- A point is unsuitable if the resulting box would overlap any
-- known boxes (typically, characters).
alreadyused :: [Box] -> [String] -> Pt -> Bool
alreadyused occupied txt pt = any (overlaps txtbox) occupied
  where txtbox = txt2box pt txt

-- A point is unsuitable if it appears "before" (in traditional
-- comic reading order) another point which is already fixed.
-- TODO: Use bounding boxes instead of centre points. Upper bound
-- should be below the lowest bound of all previous points.
outoforder :: [Pt] -> Pt -> Bool
outoforder previous this = any (this `before`) previous
  where before (x1,y1) (x2,y2) = y1 < y2 || abs (y2 - y1) < 20

-- Given an arm we move along its length until we find a position
-- that isn't *un*suitable and stop there, but we keep hold of
-- the unexplored regions in case they are needed.
-- If we can't find a suitable candidate then we return Nothing
-- and hope that other elements can be moved around to make room
-- for this one (ie backtracking).
findcandidate :: [Box] -> [String] -> [Pt] -> [Arm] -> Maybe (Pt,[Arm])
findcandidate _ _ _ [] = Nothing
findcandidate boxs txt prev (arm:arms) =
  case dropWhile unsuitable arm of
       []     -> findcandidate boxs txt prev arms
       [c]    -> Just (c,arms)
       (c:cs) -> Just (c,cs:arms)
  where unsuitable pt = alreadyused boxs txt pt || outoforder prev pt

type Path = ([String],[Arm])

-- Depth first search to find the first acceptable list of
-- locations for all text elements.
dfs :: [Box] -> [Path] -> [Path] -> [Pt] -> [Pt]
dfs _    []   _    done = done
dfs boxs todo alts done =
  let (txt,arms):rest = todo
  in case findcandidate boxs txt done arms of
       Just (pt,remains) -> dfs boxs rest ((txt,remains):alts) (pt:done)
       Nothing -> if null done || null alts -- should be same thing
                  then error "Layout:dfs couldn't place anything"
                  else dfs boxs ((head alts):todo) (tail alts) (tail done)

candidates :: [Box] -> [Path] -> [Pt]
candidates occupied potentials = dfs occupied potentials [] []

mkpaths :: Dim -> [[String]] -> [Box] -> [Path]
mkpaths sz txts speakers = map mkpath (zip txts speakers)
 where mkpath (txt,speaker) = (txt,rankarms txt sz (midpoint speaker))

-- A single speech item is the text with its location attached
-- to a speaker, with their location.
type FixedText = Located [String]
data Speech = Speech FixedText FixedPerson deriving (Eq,Show)

locatetext :: [String] -> Pt -> FixedText
locatetext txt (x,y) = Loc (Box (x - w', y - h') (x + w', y + h')) txt
  where (w,h) = estimate txt
        w' = w `div` 2
        h' = h `div` 2

-- Given the size of the background image and a list of texts with
-- known speaker locations, we return a list which identifies the
-- locations of speakers and their speeches.
fixspeeches :: Dim -> [AttributedText] -> [Speech]
fixspeeches sz attribtexts = zipWith Speech fixedtexts fixedpersons
  where positions = reverse $ candidates occupied $ mkpaths sz txts occupied
        fixedtexts = zipWith locatetext txts positions
        (txts,fixedpersons) = unzip $ map unattrib attribtexts
        occupied = map (\(Loc position _) -> position) fixedpersons
        unattrib (AttrBubble s fp) = (s,fp)
