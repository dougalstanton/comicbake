module Main where

import Control.Monad (when,(=<<))

import System.FilePath
import System.Directory
import System.Exit

import CmdArgs
import Parse
import Transform
import Pix
import Stitch

import Script -- Script
import Layout -- Speech


main = do
  opts <- processArgs
  when (null $ input opts) badFilename

  okFile <- validateFile (input opts)
  when (not okFile) unreadableFile

  loaded <- parseScriptFromFile (input opts)
  case loaded of
    Left err -> failWith err
    Right s  -> joinPanels opts =<< writePanels opts =<< genPanels s
  where badFilename = failWith "Please provide an input (script) file"
        unreadableFile = failWith "File not found or unreadable"

failWith msg = putStrLn msg >> exitFailure

validateFile filename = do
  exists <- doesFileExist filename
  readable <- readable `fmap` getPermissions filename
  return (exists && readable)

-- Transform script by bringing together the cast list with
-- character locations and choosing where to place speech bubbles.
genPanels :: Script [Panel [Action]] -> IO (Script [Panel [Speech]])
genPanels script = do
  let ctxdir = takeDirectory $ scriptLocation script
  panels <- mapM (processScene ctxdir) $ scriptContents script
  return $ script {scriptContents = panels}

-- Write temporary images, one for each panel, which will be
-- stitched together in the next stage.
writePanels :: InputOptions -> Script [Panel [Speech]] -> IO (Script [Panel FilePath])
writePanels opts script = do
  panelfiles <- mapM writeTempImage (scriptContents script)
  return $ script {scriptContents = panelfiles}
  where writeTempImage = writeImage srcdir (tmpdir opts) (output opts)
        srcdir = takeDirectory $ scriptLocation script

-- If we have more than one image we need to stitch them together.
joinPanels :: InputOptions -> Script [Panel FilePath] -> IO ()
joinPanels opts script =
  case panels of
    []      -> failWith "No comic panels to write"
    [p]     -> copyFile (action p) dst
    (_:_:_) -> writeComic dst script
  where dst = (outdir opts) </> (output opts) <.> "png"
        panels = scriptContents script
