module Pix (Pix(..), panel2pix, writePix) where

import Graphics.Rendering.Diagrams
import qualified Graphics.Rendering.Diagrams.Types as DT
import System.FilePath (combine)

import Script hiding (position)
import Layout

data Pix = Pix { bgImage :: FilePath
               , overlays :: [(DT.Point, DT.Diagram)]
	       , order :: Int
	       }

panel2pix panel = Pix { bgImage = background panel
                      , overlays = nulloverlay:map mkdiagram (bubbles panel)
		      , order = number panel
		      }

-- Diagrams can be accurately positioned with respect to
-- the top-left diagram, so we need something in the
-- corner in order to place everything else accurately.
nulloverlay = ((0,0),empty 0 0)

para sz = vcatA left . map (text sz)

mkdiagram :: ([String],Frame) -> (DT.Point, DT.Diagram)
mkdiagram (str,((x,y):_)) = ((fromIntegral x,fromIntegral y),para 12 str)

-- renderOverlayPNG <- srcfile dstfile diagram
writePix dir pix = renderOverPNG (dir `combine` bgImage pix) fn d
 where d = position $ overlays pix
       fn = "frame" ++ show (order pix) ++ ".png"
