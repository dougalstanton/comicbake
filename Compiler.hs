module Main where

import Control.Monad (liftM)
import System.FilePath (takeDirectory, replaceExtension, (</>))

import Script
import CastList

import ScriptParse
import ImageMapParse

readScript :: FilePath -> IO (Script [Scene])
readScript filename = parseScript `liftM` readFile filename
    where parseScript = rawToScenes . flip rawScript path
          path = takeDirectory filename

readImageMap :: FilePath -> IO CastList
readImageMap filename = parseImageMap `liftM` readFile filename

stage3 script = do castlists <- mapM readImageMap (imgmaps script)
                   return $ fmap (\scenes -> zipWith stageN scenes castlists) script

    where   imageMapName = (</>) (scriptLocation script) . flip replaceExtension "map"
            imgmaps = map (imageMapName . sceneBackground) . scriptContents

actorsInPlace :: CastList -> Speech -> Cue
actorsInPlace list speech = Cue { cueActor = fst speech
                                , cueWords = snd speech
                                , cueCoord = getCoords (fst speech) list
                                }

stageN :: Scene -> CastList -> Scene
stageN scene castlist = scene { sceneDialogue = map (shift (actorsInPlace castlist)) dialogue }
        where shift :: (b -> a) -> Either a b -> Either a b
              shift f (Right r) = Left (f r)
              dialogue = sceneDialogue scene
