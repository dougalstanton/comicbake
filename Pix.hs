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

writeImage :: FilePath -> FilePath -> Panel [Speech] -> IO (Panel FilePath)
writeImage directory prefix panel = do
  img <- loadPngFile (background panel)
  foldM addspeech img (action panel)
  let outputfile = directory </> prefix <.> show (number panel) <.> "png"
  savePngFile outputfile img
  return (panel {action = outputfile })
