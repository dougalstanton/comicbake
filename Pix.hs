module Pix where

import Control.Monad
import Graphics.GD
import System.FilePath

import Script
import Sanitise
import Locations
import Layout
import Drawing

addspeech off img (Speech (Loc txtbox txt) (Loc speakerbox _)) = do
  speechbubble txt (off += midpoint txtbox) (off += midpoint speakerbox) img
  return img
  where (+=) v (x,y) = (v+x,v+y)

writeImage :: FilePath -> FilePath -> FilePath -> Panel [Speech] -> IO (Panel FilePath)
writeImage srcdir destdir filename panel = do
  img <- expandImage =<< loadPngFile (srcdir </> (background panel))
  foldM (addspeech 10) img (action panel)
  let imgfile = destdir </> filename <.> show (number panel) <.> "png"
  savePngFile imgfile img
  return (panel {action = imgfile})

expandImage :: Image -> IO Image
expandImage src = do
  (width,height) <- imageSize src
  dst <- newImage (width+20,height+20)
  fillImage (rgb 0 0 0) dst
  copyRegion (0,0) (width,height) src (10,10) dst
  return dst
