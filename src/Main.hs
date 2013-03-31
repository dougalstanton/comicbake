module Main where

import Control.Monad (when)

import Data.Maybe (fromMaybe)

import System.FilePath
import System.Directory
import System.Exit

import CmdArgs
import Parsing.ScriptParse
import Comic.Transform
import Publish.Flickr
import Graphics.Pix
import Graphics.Stitch

import Script -- Script
import Comic.Layout -- Speech

main :: IO ()
main = execParser options >>= go
  where go (UploadComic comic) = publishComic comic
        go (MkComic    script) = buildComic script

buildComic :: Build -> IO ()
buildComic opts = do
  when (null $ input opts) badFilename

  okFile <- validateFile (input opts)
  when (not okFile) $ unreadableFile (input opts)

  loaded <- parseScriptFromFile (input opts)
  case loaded of
    Left err -> failWith err
    Right s  -> joinPanels opts =<< writePanels opts =<< genPanels s
  where badFilename = failWith "Please provide an input (script) file"

publishComic :: Publish -> IO ()
publishComic opts = do
  let login = enableFlickr opts
      logout = disableFlickr opts
  when (login && logout)
    (failWith "Can't use enableFlickr and disableFlickr together.")
  when login (flickrLogin >> exitSuccess)
  when logout (flickrLogout >> exitSuccess)

  okFile <- validateFile (comicstrip opts)
  when (not okFile) $ unreadableFile (comicstrip opts)
  let info = Info { title = fromMaybe "Unknown comic" $ cli_title opts
                  , description = fromMaybe "" $ cli_desc opts
                  , author = "", date = "", credits = []
                  , tags = cli_tags opts
                  }
  murl <- flickrUpload $ ULData (comicstrip opts) info
  putStrLn $ maybe "Image could not be published."
                   ("Image successfully uploaded to:\n" ++) murl

-- Error messages and quick exit on failure.
failWith msg = putStrLn msg >> exitFailure
unreadableFile f = failWith $ "File "++ show f ++" not found or unreadable"

validateFile :: FilePath -> IO Bool
validateFile filename = do
  exists <- doesFileExist filename
  isreadable <- readable `fmap` getPermissions filename
  return (exists && isreadable)

-- Transform script by bringing together the cast list with
-- character locations and choosing where to place speech bubbles.
genPanels :: Script [Panel [Action]] -> IO (Script [Panel [Speech]])
genPanels script = do
  let ctxdir = takeDirectory $ scriptLocation script
  panels <- mapM (processScene ctxdir) $ scriptContents script
  return $ script {scriptContents = panels}

-- Write temporary images, one for each panel, which will be
-- stitched together in the next stage.
writePanels :: Build -> Script [Panel [Speech]] -> IO (Script [Panel FilePath])
writePanels opts script = do
  panelfiles <- mapM writeTempImage (scriptContents script)
  let imgwidth = maximum $ map (maybe (error "Unknown size!") fst . bgsize) panelfiles
  headerfile <- writeHeader (title $ scriptInfo script) imgwidth (tmpdir opts) (output opts)
  footerfile <- writeFooter (author $ scriptInfo script) imgwidth (tmpdir opts) (output opts)
  let h = Panel 0 "" Nothing headerfile
      f = Panel 0 "" Nothing footerfile
      allfiles = [h] ++ panelfiles ++ [f]
  return $ script {scriptContents = allfiles}
  where writeTempImage = writeImage srcdir (tmpdir opts) (output opts)
        srcdir = takeDirectory $ scriptLocation script

-- If we have more than one image we need to stitch them together.
joinPanels :: Build -> Script [Panel FilePath] -> IO ()
joinPanels opts script =
  case panels of
    []      -> failWith "No comic panels to write"
    [p]     -> copyFile (action p) dst
    (_:_:_) -> writeComic dst script
  where dst = (outdir opts) </> (output opts) <.> "png"
        panels = scriptContents script
