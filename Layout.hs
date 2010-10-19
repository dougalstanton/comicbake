module Layout where

import Control.Arrow (first,second)

import Script
import Locations

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
estimate str = (divrt2 w,h)
  where w = fromIntegral $ maximum $ map length str
        h = length str
        divrt2 = round . (/sqrt 2) . fromIntegral

-- Create radial arms made of candidate centre-points.
-- Various diagonals can be made by joining the two
-- other arms to take the average diagonal line.
joinarms arm1 arm2 pt = zipWith f (arm1 pt) (arm2 pt)
  where f (x1,y1) (x2,y2) = (x1+x2,y1+y2) 
-- (0,0) is top left, x++ moves right, y++ moves down
trail c f = iterate (c f)
horiz :: (Int -> Int) -> Pt -> [Pt]
horiz f = trail first f
vertic :: (Int -> Int) -> Pt -> [Pt]
vertic f = trail second f

-- We don't want to choose locations that are way off screen,
-- but a little bit of overspill avoids the boxed-in feel.
inbounds (w,h) (x,y) = x >= 0 && x < w && y >= 0 && y < h

-- Arms heading in the general direction of the centre
-- of the scene are probably aesthetically nicer (guesswork).
rankarms :: Dim -> Pt -> [Pt]
rankarms sz@(pwidth,pheight) (x,y) = undefined
  where (cx,cy) = (pwidth`div`2, pheight`div`2)
        keep = takeWhile (inbounds sz)
        --hdir = if cx > x then succ else pred
        --vdir = if cy > y then succ else pred
