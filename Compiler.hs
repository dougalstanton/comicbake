module Main where

import Control.Monad (liftM)
import System.FilePath (takeDirectory, replaceExtension, combine)
import System.Environment
import Data.Char (toLower)

import Graphics.GD (loadPngFile, imageSize)

import Script
import Parse
import Layout
import Pix

import CastList
import ImageMapParse

readImageMap :: FilePath -> FilePath -> IO CastList
readImageMap rootdir filename = parseImageMap `liftM` readFile location
    where location = rootdir `combine` filename

imageMapFromScene :: Scene -> FilePath
imageMapFromScene scene = replaceExtension imgfile "map"
    where imgfile = sceneBackground scene

mapsFromScript :: FilePath -> Script [Scene] -> IO [CastList]
mapsFromScript dir = mapM (readImageMap dir . imageMapFromScene) . scriptContents

useCoords :: CastList -> Scene -> Scene
useCoords cl s = s { sceneAction = map (annotateActionFrame cl) actions}
    where actions = sceneAction s

annotateActionFrame :: CastList -> Action -> Action
annotateActionFrame castlist a = a {position = getCoords c castlist}
    where c = map toLower $ character a

imageSizesFromScript :: FilePath -> Script [Scene] -> IO [Dim]
imageSizesFromScript ctxdir = mapM (getImgSize ctxdir) . scriptContents

getImgSize :: FilePath -> Scene -> IO Dim
getImgSize ctxdir scene = loadPngFile imgfile >>= imageSize
  where imgfile = ctxdir `combine` sceneBackground scene

main :: IO ()
main = do [scriptfile] <- getArgs
          res <- parseScriptFromFile scriptfile
          case res of
            Left e  -> putStr e
            Right s -> f s
  where f s = do let rootdir = takeDirectory $ scriptLocation s
  		 maps <- mapsFromScript rootdir s
		 let s1 = fmap (zipWith useCoords maps) s
		 bgsizes <- imageSizesFromScript rootdir s1
		 let s2 = fmap (zipWith scene2panel bgsizes) s1
		 let s3 = fmap (map panel2pix) s2
		 writeall rootdir s3

writeall :: FilePath -> Script [Pix] -> IO ()
writeall ctxdir = mapM_ (writePix ctxdir) . scriptContents
