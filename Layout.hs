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
-- number of lines, and characters per line. Divide the width by
-- sqrt 2 (guesswork) because many characters are not full width.
-- Assume a 20px character size.
estimate str = (divrt2 w,h)
  where w = 20 * (maximum $ map length str)
        h = 20 * length str
        divrt2 = round . (/sqrt 2) . fromIntegral

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

-- We don't want to choose locations that are way off screen,
-- but a little bit of overspill avoids the boxed-in feel.
inbounds (w,h) (x,y) = x >= 0 && x < w && y >= 0 && y < h

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
rankarms :: Dim -> Pt -> [Arm]
rankarms sz@(pw,ph) pt@(x,y) = map keep candidates
  where keep = takeWhile (inbounds sz)
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

-- A point is unsuitable if the resulting box would overlap any
-- known boxes (typically, characters).
alreadyused :: [Box] -> [String] -> Pt -> Bool
alreadyused occupied txt (x,y) = any (overlaps txtbox) occupied
  where txtbox = Box (x-w, y-h) (x+w, y+h)
        (w,h) = half *** half $ estimate txt
        half = flip div 2

-- A point is unsuitable if it appears "before" (in traditional
-- comic reading order) another point which is already fixed.
outoforder :: [Pt] -> Pt -> Bool
outoforder previous this = any (`after` this) previous
  where after (x1,y1) (x2,y2) = y2 < y1 || x2 < x1

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

mkpaths :: Dim -> [[String]] -> [Pt] -> [Path]
mkpaths sz txts speakers = map mkpath (zip txts speakers)
 where mkpath (txt,speaker) = (txt,rankarms sz speaker)
