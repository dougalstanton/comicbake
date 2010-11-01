module Drawing where

import Graphics.GD

-- Create various shapes which are used to put together
-- comic features --- speech bubbles, bordered frames,
-- description boxes and so on.

-- Constants
fontsize = 10

-- Common colours.
white = rgb 255 255 255
black = rgb 0 0 0

-- Useful functions
half n = div n 2
quarter = half . half

-- Freetype bounds not very reliable
-- coordinates in order: bl br tr tl
boundsize (a,b,c,d) = (6 + maxx - minx, 6 + maxy - miny)
  where bounds = [a,b,c,d]
        maxx = maximum $ map fst bounds
        minx = minimum $ map fst bounds
        maxy = maximum $ map snd bounds
        miny = minimum $ map snd bounds

-- Draw wide oval around a rectangle of the given width and height.
oval (w,h) pt = \img -> do
  drawFilledEllipse pt sz white img
  where sz = (adj w, adj h)
        adj n = round (2 * fromIntegral n / sqrt 2)

-- Draw rectangle of the given width and height.
rect (w,h) (x,y) = drawFilledRectangle pt1 pt2 white
  where pt1 = (x - half w, y - half h)
        pt2 = (x + half w, y + half h)

-- Very handy for debugging on the final image.
dot (x,y) = drawFilledRectangle (x,y) (succ x,succ y) (rgb 255 0 0)

-- Draw the supplied string list centred on this position.
text name strs (x,y) = \img -> do
  let str = init $ unlines strs -- drop that last \n
  -- drawString takes the lower-left co-ordinate of the first letter
  -- as its starting point. Can you believe this nonsense?
  -- Anyway, we have to add the height of the first line back on.
  firstline <- measureString name fontsize 0 (x,y) (head strs) black
  bounds <- measureString name fontsize 0 (x,y) str black
  let (_,fontheight) = boundsize firstline
      (w,h) = boundsize bounds
      pt = (x - half w, fontheight + y - half h)
  drawString name fontsize 0 pt str black img
  return (w,h)

-- Draw a tail which curves from the start point in the general
-- direction of the destination.
curvetail sz pt dst = drawArc pt sz start end white
  where start = 0; end = const 45 dst

-- Draw smaller blobs between the start and end points.
blobtail (width,height) (sx,sy) (dx,dy) img =
  mapM_ (\(sz,pt,_) -> oval sz pt img) $ take 3 $ iterate smaller firstblob
  where (vecx,vecy) = ((dx-sx)`div`3,(dy-sy)`div`3)
        smaller ((w,h),(x,y),(vx,vy)) =
                ((half w,half h), (x+vx,y+vy), (half vx,half vy))
        firstblob = ((quarter width,quarter height),
                     (sx+vecx,sy+vecy),(half  vecx, half vecy))

-- Create a thought bubble with a trail of smaller bubbles.
thoughtbubble strs pt dst img = do
  useFontConfig True
  -- We need to scale the bubbles around the text, so first
  -- find out how large the text will be.
  dims <- text "Sans" strs pt img
  oval dims pt img
  blobtail dims pt dst img
  text "Sans" strs pt img
  return ()

-- Create a speech bubble with a tail pointing at the speaker.
speechbubble strs pt dst img = do
  useFontConfig True
  dims <- text "Sans" strs pt img
  rect dims pt img
  -- TODO: add in tail pointing at speaker
  text "Sans" strs pt img
  return ()
