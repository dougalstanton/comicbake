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

processScene :: Scene -> IO (Panel [Speech])
processScene scene = do
  let panelobjs = fmap gatherobjects $ sanitiseScene scene
  panelattrtext <- loadCastList panelobjs
  bgImageSize panelattrtext

-- Load and parse the cast list from the .map file.
loadCastList :: Panel Objects -> IO (Panel [AttributedText])
loadCastList panel = do
  let imagemapfile = background panel `replaceExtension` "map"
  castlist <- parseImageMap `fmap` readFile imagemapfile
  return $ fmap (convert castlist) panel

-- Load the background image to get its dimensions.
-- TODO: Store the image data rather than fetching it twice.
bgImageSize :: Panel [AttributedText] -> IO (Panel [Speech])
bgImageSize panel = do
  dims <- (loadPngFile (background panel) >>= imageSize)
  return $ fmap (fixspeeches dims) panel
