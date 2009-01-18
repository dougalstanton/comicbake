module Main where

import Control.Monad (liftM)
import System.FilePath (takeDirectory)

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
