module Stitch (writeComic) where

import Graphics.Transform.Magick.Images
import Graphics.Transform.Magick.Types

import Script
import Sanitise

-- Join all the panels together and write them out to
-- a single file.
-- TODO: Let user choose vertical/horizontal/tabular
-- panel layout.
-- TODO: Pad images with some pleasing margins.
writeComic :: FilePath -> Script [Panel FilePath] -> IO ()
writeComic outputfile script = do
  let panels = scriptContents script
      orientation = TopToBottom
  joinComics orientation (map action panels) outputfile

joinComics orientation srcs dst = do
  imgs <- mapM readImage srcs
  writeImage dst (appendImages orientation imgs)
