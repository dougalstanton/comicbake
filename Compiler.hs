module Main where

import Control.Monad (liftM)
import System.FilePath (takeDirectory, replaceExtension, combine)
import System.Environment

import Script
import CastList

import Parse
import ImageMapParse

readScript :: FilePath -> IO (Script [Scene])
readScript filename = parseScript `liftM` readFile filename
    where parseScript = rawToScenes . flip rawScript path
          path = takeDirectory filename

readImageMap :: FilePath -> IO CastList
readImageMap filename = parseImageMap `liftM` readFile filename

-- | Look up image maps and place details of actors'
--   locations into the script.
stage3 :: Script [Scene] -> IO (Script [Scene])
stage3 script = do castlists <- mapM readImageMap (imgmaps script)
                   return $ fmap (zipWith stageN castlists) script

    where   imageMapName = combine (scriptLocation script) . flip replaceExtension "map"
            imgmaps = map (imageMapName . sceneBackground) . scriptContents

actorsInPlace :: CastList -> Speech -> Cue
actorsInPlace list speech = Cue { cueActor = fst speech
                                , cueWords = snd speech
                                , cueCoord = getCoords (fst speech) list
                                }

-- | Given a complete list of cast locations for a scene
--   replace every instance of a bare speech in the scene
--   with the annotated equivalent.
stageN :: CastList -> Scene -> Scene
stageN castlist scene = scene { sceneDialogue = map (shift (actorsInPlace castlist)) dialogue }
        where shift :: (b -> a) -> Either a b -> Either a b
              shift f (Right r) = Left (f r)
              dialogue = sceneDialogue scene


main = getArgs >>= readScript . head >>= stage3 >>= print

