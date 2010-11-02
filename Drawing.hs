module Drawing where

import Graphics.GD

-- Create various shapes which are used to put together
-- comic features --- speech bubbles, bordered frames,
-- description boxes and so on.

-- Constants
fontsize = 10

-- Common colours.
white = rgb 255 255 255
grey  = rgb 200 200 200
black = rgb 0 0 0

-- Useful functions
half n = div n 2
quarter = half . half
pair f (a,b) = (f a,f b)
scale f = round . f . fromIntegral
scaleXY x y (a,b) = (scale (*x) a, scale (*y) b)

-- Freetype bounds not very reliable
-- coordinates in order: bl br tr tl
boundsize (a,b,c,d) = (maxx - minx, maxy - miny)
  where bounds = [a,b,c,d]
        maxx = maximum $ map fst bounds
        minx = minimum $ map fst bounds
        maxy = maximum $ map snd bounds
        miny = minimum $ map snd bounds

-- Draw wide oval around a rectangle of the given width and height.
oval :: Size -> Point -> Image -> IO ()
oval sz pt = \img -> do
  drawFilledEllipse pt (pair adjust sz) white img
  where adjust = scale (\x -> 2 * x / sqrt 2)

-- Draw a box with border around this point
frame :: Size -> Point -> Image -> IO ()
frame (w,h) pt = \img -> do
  rect (w+2,h+2) pt grey img
  rect (w,h) pt white img

-- Draw rectangle of the given width and height centred on (x,y)
rect :: Size -> Point -> Color -> Image -> IO ()
rect (w,h) (x,y) colour = drawFilledRectangle pt1 pt2 colour
  where pt1 = (x - half w, y - half h)
        pt2 = (x + half w, y + half h)

-- Very handy for debugging on the final image.
dotR,dotB :: Point -> Image -> IO ()
dotR (x,y) = drawFilledRectangle (x,y) (succ x,succ y) (rgb 255 0 0)
dotB (x,y) = drawFilledRectangle (x,y) (succ x,succ y) (rgb 0 0 255)

-- Draw the supplied string list centred on this position.
text :: String -> [String] -> Point -> Image -> IO Point
text name strs (x,y) = \img -> do
  let str = init $ unlines strs -- drop that last \n
  -- drawString takes the lower-left co-ordinate of the first letter
  -- as its starting point. Can you believe this nonsense?
  -- Anyway, we have to add the height of the first line back on.
  firstline <- measureString name fontsize 0 (x,y) (head strs) black
  bounds <- measureString name fontsize 0 (x,y) str black
  let (_,fontheight) = boundsize firstline
      (w,h) = boundsize bounds
      fudge = scale (*0.7) fontheight
      pt = (x - half w, fudge + y - half h)
  drawString name fontsize 0 pt str black img
  return (w,h)

-- Draw a tail which curves from the start point in the general
-- direction of the destination.
curvetail :: Size -> Point -> Point -> Image -> IO ()
curvetail sz pt dst = drawArc pt sz start end white
  where start = 0; end = const 45 dst

-- Draw smaller blobs between the start and end points.
blobtail :: Size -> Point -> Point -> Image -> IO ()
blobtail (width,height) (sx,sy) (dx,dy) img =
  mapM_ (\(sz,pt,_) -> oval sz pt img) $ take 3 $ iterate smaller firstblob
  where (vecx,vecy) = ((dx-sx)`div`3,(dy-sy)`div`3)
        smaller ((w,h),(x,y),(vx,vy)) =
                ((half w,half h), (x+vx,y+vy), (half vx,half vy))
        firstblob = ((quarter width,quarter height),
                     (sx+vecx,sy+vecy),(half  vecx, half vecy))

-- Create a thought bubble with a trail of smaller bubbles.
thoughtbubble :: [String] -> Point -> Point -> Image -> IO ()
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
speechbubble :: [String] -> Point -> Point -> Image -> IO ()
speechbubble strs pt dst img = do
  useFontConfig True
  dims <- text "Sans" strs pt img
  frame (scaleXY 1.2 1.5 dims) pt img
  -- TODO: add in tail pointing at speaker
  text "Sans" strs pt img
  return ()
