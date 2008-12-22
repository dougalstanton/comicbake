module Main where

import Control.Monad (liftM)
import System.FilePath (takeDirectory)

import Script

import ScriptParse

readScript :: FilePath -> IO (Script [Scene])
readScript filename = parseScript `liftM` readFile filename
    where parseScript = rawToScenes . flip rawScript path
          path = takeDirectory filename
