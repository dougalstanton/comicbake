{-# LANGUAGE DeriveDataTypeable #-}
module CmdArgs (InputOptions(..), processArgs) where

import System.Console.CmdArgs

data InputOptions
  = Build { output :: FilePath
          , outdir :: FilePath
          , tmpdir :: FilePath
          , input  :: FilePath }
  | Publish { comicstrip    :: FilePath
            , cli_title     :: Maybe String
            , cli_desc      :: Maybe String
            , cli_tags      :: [String]
            , enableFlickr  :: Bool
            , disableFlickr :: Bool}
  deriving (Eq,Show,Data,Typeable)

defOutFile = "comicstrip"

buildOpts = Build
  { outdir = "." &= typDir
    &= help "Output directory (default: current directory)"
  , tmpdir = "." &= typDir
    &= help "Temp data directory (default: current directory)"
  , output = defOutFile &= name "o" &= typFile
    &= help ("Name of output file (default: " ++ defOutFile ++ ")")
  , input  = def &= typFile
    &= help "Script to convert"
  } &= help "Build a comic strip from a script"

publishOpts = Publish
  { comicstrip = (defOutFile ++ ".png") &= typFile
    &= help "Path to comic strip image"
  , cli_title = def &= typ "TITLE"
    &= help "Comic strip title"
  , cli_desc = def &= typ "DESC"
    &= help "Description of this comic"
  , cli_tags = def &= typ "TAG1,TAG2,.."
    &= help "Tags to annotate this comic"
  , enableFlickr = False &= help "Enable Flickr without uploading anything"
  , disableFlickr = False &= help "Disable Flickr"
  }
  &= help "Upload a comic strip"

options = modes [buildOpts &= auto, publishOpts]
  &= program "comicbake"
  &= summary "ComicBake v0.2, (c) Dougal Stanton 2010-11"
  &= help "Convert text scripts into web comics"

processArgs = cmdArgs options
