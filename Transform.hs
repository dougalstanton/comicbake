module Transform where

import Control.Monad

import System.FilePath
import Graphics.GD

import Script
import Parse
import Sanitise
import Objects
import Locations
import Layout
import CastList
import ImageMapParse

processScene :: FilePath -> Scene -> IO (Panel [Speech])
processScene dir scene = do
  let panelobjs = fmap gatherobjects $ sanitiseScene scene
  panelattrtext <- loadCastList dir panelobjs
  bgImageSize dir panelattrtext

-- Load and parse the cast list from the .map file.
loadCastList :: FilePath -> Panel Objects -> IO (Panel [AttributedText])
loadCastList dir panel = do
  let imagemapfile = dir </> background panel `replaceExtension` "map"
  castlist <- parseImageMap `fmap` readFile imagemapfile
  return $ fmap (convert castlist) panel

-- Load the background image to get its dimensions.
-- TODO: Store the image data rather than fetching it twice.
bgImageSize :: FilePath -> Panel [AttributedText] -> IO (Panel [Speech])
bgImageSize dir panel = do
  dims <- (loadPngFile (dir </> background panel) >>= imageSize)
  return $ fmap (fixspeeches dims) panel
