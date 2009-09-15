module Pix (Pix(..), panel2pix, writePix) where

import Graphics.Rendering.Diagrams
import qualified Graphics.Rendering.Diagrams.Types as DT
import System.FilePath (combine)
import Control.Arrow ((***))

import Script (Script(..), IsFrame(..), Frame, Pt)
import Layout (Panel(..), Bubble(..))

data Pix = Pix { bgImage :: FilePath
               , overlays :: [(DT.Point, DT.Diagram)]
	       , order :: Int
	       }

type Dim = (Int,Int)

panel2pix :: Panel -> Pix
panel2pix panel = Pix { bgImage = background panel
                      , overlays = corner:diagrams
		      , order = number panel
		      }
 where diagrams = map mkdiagram (bubbles panel) ++ map mkdebug (bubbles panel)

-- Diagrams can be accurately positioned with respect to
-- the top-left diagram, so we need something in the
-- corner in order to place everything else accurately.
corner :: (DT.Point, DT.Diagram)
corner = ((0,0),empty 0 0)

underlay a b = a `withSize` b ## b
above a b = let a' = a `withSize` b in vcatA hcenter [a', b]
below a b = let a' = a `withSize` b in vcatA hcenter [b,a']
para sz = vcatA left . map (text sz)

plainbubble :: [String] -> Diagram
plainbubble strs = bubble `underlay` writing
 where bubble w h = fc white $ lc white $ roundRect (w*1.2) (h*1.2)
       writing = para 12 strs

entail :: DT.Point -> DT.Point -> Diagram -> Diagram
entail (fx,fy) (bx,by) = if by>fy then above tail else below tail
 where tail w h = fc white $ lc white $ curved 0 $ pathFromVertices $ map transform $ map (\(x,y) -> (x*w/3, y*h/2)) pts
       transform = ((if bx>fx then negate else id) *** (if by<fy then negate else id))
       pts = [(0,0),(1,-1),(1,0)]

mkdebug :: Bubble -> (DT.Point, DT.Diagram)
mkdebug b = (fi2 (loc b), uncurry rect (fi2 (fakesize b)))
 where fi2 = fromIntegral *** fromIntegral

mkdiagram :: Bubble -> (DT.Point, DT.Diagram)
mkdiagram b = (bloc, entail floc bloc (plainbubble (content b)))
   where floc = ((/2) . fi *** (/2) .fi) $ foldr sumpair (0,0) $ anchor b
         bloc = (fi *** fi) $ loc b
	 fi = fromIntegral
	 sumpair (x1,y1) (x2,y2) = (x1+x2,y1+y2)

-- renderOverlayPNG <- srcfile dstfile diagram
writePix dir pix = renderOverPNG (dir `combine` bgImage pix) fn d
 where d = position $ overlays pix
       fn = "frame" ++ show (order pix) ++ ".png"
