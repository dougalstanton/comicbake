module Main where

import Control.Monad (liftM)
import System.FilePath (takeDirectory, replaceExtension, combine)
import System.Environment
import Data.Char (toLower)

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

main :: IO ()
main = do [scriptfile] <- getArgs
          res <- parseScriptFromFile scriptfile
          case res of
            Left e  -> putStr e
            Right s -> f s
  where f s = do let rootdir = takeDirectory $ scriptLocation s
                 maps <- mapsFromScript rootdir s
                 let s' = fmap (zipWith useCoords maps) s
		     s''= fmap (map (panel2pix . scene2panel)) s'
		 writeall s''

writeall :: Script [Pix] -> IO ()
writeall script = mapM_ (writePix ctxdir) $ scriptContents script
 where ctxdir = takeDirectory $ scriptLocation script
