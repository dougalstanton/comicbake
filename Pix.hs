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
                      , overlays = drawHeadBoxes panel ++ drawBubbleBoxes panel
		      , order = number panel
		      }

writePix :: FilePath -> Pix -> IO ()
writePix rootdir pix = do
  let filename = "frame" ++ show (order pix) ++ ".png"
  let actions = map action . overlays $ pix
  imgptr <- loadPngFile (rootdir </> bgImage pix)
  forM_ actions ($imgptr)
  savePngFile filename imgptr

--
--
--
drawBubbleBoxes :: Panel -> [ImgWriter]
drawBubbleBoxes = map (ImgAction . drawEllipse . area) . bubbles

drawHeadBoxes :: Panel -> [ImgWriter]
drawHeadBoxes = map (ImgAction . drawBox . anchor) . bubbles

drawBox :: Box -> Image -> IO ()
drawBox box = drawFilledRectangle (topleft box) (bottomright box) white
 where white = rgb 255 255 255

drawEllipse :: Box -> Image -> IO ()
drawEllipse box = drawFilledEllipse (midpoint box) (dim box) red
 where red = rgb 255 0 0
