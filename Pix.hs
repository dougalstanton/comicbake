module Pix (Pix(..), panel2pix, writePix) where

import Graphics.Rendering.Diagrams
import qualified Graphics.Rendering.Diagrams.Types as DT
import System.FilePath (combine)
import Control.Arrow ((***))

import Script hiding (position)
import Layout

data Pix = Pix { bgImage :: FilePath
               , overlays :: [(DT.Point, DT.Diagram)]
	       , order :: Int
	       }

type Dim = (Int,Int)

panel2pix panel = Pix { bgImage = background panel
                      , overlays = corner:map mkdiagram (bubbles panel)
		      , order = number panel
		      }

-- Diagrams can be accurately positioned with respect to
-- the top-left diagram, so we need something in the
-- corner in order to place everything else accurately.
corner :: (DT.Point, DT.Diagram)
corner = ((0,0),empty 0 0)

underlay a b = a `withSize` b ## b
atop a b = vcatA hcenter [(a `withSize` b), b]
para sz = vcatA left . map (text sz)

plainbubble :: [String] -> Diagram
plainbubble strs = bubble `underlay` writing
 where bubble w h = fc white $ lc white $ roundRect (w*1.2) (h*1.2)
       writing = para 12 strs

entail :: DT.Point -> DT.Point -> Diagram -> Diagram
entail (fx,fy) (bx,by) = atop tail
 where tail w h = fc white $ lc white $ curved 0 $ pathFromVertices $ map transform $ map (\(x,y) -> (x*w/3, y*h/2)) pts
       transform = (if bx>fx then negate else id) *** (if by>fy then negate else id)
       pts = [(-0.25,0),(-1,-1),(0.25,0)]

mkdiagram :: ([String],Frame,Dim) -> (DT.Point, DT.Diagram)
mkdiagram (str,((x,y):_),(x',y')) =
  (bb, entail fr bb (plainbubble str))
   where fr@(fx,fy) = (fromIntegral x', fromIntegral y')
         bb@(bx,by) = (fromIntegral x, fromIntegral y)

-- renderOverlayPNG <- srcfile dstfile diagram
writePix dir pix = renderOverPNG (dir `combine` bgImage pix) fn d
 where d = position $ overlays pix
       fn = "frame" ++ show (order pix) ++ ".png"
