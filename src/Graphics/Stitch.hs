module Graphics.Stitch (writeComic) where

import Filesystem.Path.CurrentOS hiding (FilePath)
import Graphics.ImageMagick.MagickWand

import Script

-- Join all the panels together and write them out to
-- a single file.
-- TODO: Let user choose vertical/horizontal/tabular
-- panel layout.
-- TODO: Pad images with some pleasing margins.
writeComic :: FilePath -> Script [Panel FilePath] -> IO ()
writeComic outputfile script = do
  let panels = scriptContents script
      vertical = True
  joinComics vertical (map action panels) outputfile

joinComics orientation srcs dst = withMagickWandGenesis $ do
  (_,w) <- magickWand
  resetIterator w
  mapM_ (readImage w . decodeString) srcs
  resetIterator w
  (_, result) <- (appendImages w orientation)
  writeImage result $ Just $ decodeString dst
