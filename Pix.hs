module Pix where

import Graphics.GD
import System.FilePath ((</>))
import Control.Monad (forM_)

import Script (IsFrame(..), Box(..))
import Layout (Panel(..), Bubble(..))

data ImgWriter = ImgAction { action :: (Image -> IO ()) }

data Pix = Pix { bgImage  :: FilePath
               , overlays :: [ImgWriter]
	       , order    :: Int
	       }

panel2pix :: Panel -> Pix
panel2pix panel = Pix { bgImage = fst $ background panel -- ignore size
                      , overlays = drawBubbleBoxes panel
		      , order = number panel
		      }

writePix :: FilePath -> Pix -> IO ()
writePix rootdir pix = do
  let filename = "frame" ++ show (order pix) ++ ".png"
  let actions = map action . overlays $ pix
  useFontConfig True
  imgptr <- loadPngFile (rootdir </> bgImage pix)
  forM_ actions ($imgptr)
  savePngFile filename imgptr

--
--
--
drawBubbleBoxes :: Panel -> [ImgWriter]
drawBubbleBoxes = map drawBubble . bubbles

drawHeadBoxes :: Panel -> [ImgWriter]
drawHeadBoxes = map (drawRect . anchor) . bubbles

-- Wrapped shapes
drawEllipse :: Box -> ImgWriter
drawEllipse = ImgAction . drawEllipse_
drawRect :: Box -> ImgWriter
drawRect = ImgAction . drawRect_
drawBubble :: Bubble -> ImgWriter
drawBubble = ImgAction . drawBubble_
--
-- Draw individual shapes.
--
drawRect_ :: Box -> Image -> IO ()
drawRect_ box = drawFilledRectangle tl br white
  where white = rgb 255 255 255
        tl = topleft box
        br = bottomright box

drawEllipse_ :: Box -> Image -> IO ()
drawEllipse_ box = drawFilledEllipse mp sz red
  where red = rgb 255 0 0
        mp = midpoint box
        sz = dim box

drawBubble_ :: Bubble -> Image -> IO ()
drawBubble_ bbl = \img -> do
  let oldorigin@(oldx,oldy) = loc bbl
  ((x1,y1),(x2,y2)) <- drawParagraph Nothing oldorigin (content bbl)
  let (dx,dy) = (oldx - x1, oldy - y1)
      (w,h) = (x2-x1,y2-y1)
  -- draw a bubble
  drawFilledRectangle (oldx-3,oldy-3) (oldx+w+3,oldy+h+3) (rgb 255 255 255) img
  -- draw the words
  _ <- drawParagraph (Just img) (oldx+dx,oldy+dy) (content bbl)
  return ()


-- Point,Point is really a frame representing the top left
-- and lower right extents of the paragraph of text: data
-- we'll need to wrap it in a bubble.
-- Don't pass in an image if you just want to get measurements.
drawParagraph :: Maybe Image -> Point -> [String] -> IO (Point,Point)
drawParagraph mimg origin strs = do
  (_,lr,_,ul) <- drawText origin (unlines strs) mimg
  return (ul,lr)

-- Argh, the given location is the left edge of the text baseline :(
drawText :: Point -> String -> Maybe Image -> IO (Point,Point,Point,Point)
drawText pt str img = maybe fakestring realstring img
  where font = "ttf-dejavu/DejaVuSans"
        ptsize = 8
	angle = 0
	black = rgb 0 0 0
        realstring = drawString font ptsize angle pt str black
        fakestring = measureString font ptsize angle pt str black
