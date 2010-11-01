module Pix where

import Control.Monad
import Graphics.GD
import System.FilePath

import Script
import Sanitise
import Locations
import Layout
import Drawing

addspeech img (Speech (Loc txtbox txt) (Loc speakerbox _)) = do
  speechbubble txt (midpoint txtbox) (midpoint speakerbox) img
  return img

writeImage :: FilePath -> FilePath -> FilePath -> Panel [Speech] -> IO (Panel FilePath)
writeImage srcdir destdir filename panel = do
  img <- loadPngFile (srcdir </> (background panel))
  foldM addspeech img (action panel)
  let imgfile = destdir </> filename <.> show (number panel) <.> "png"
  savePngFile imgfile img
  return (panel {action = imgfile})
