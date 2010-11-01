{-# LANGUAGE DeriveDataTypeable #-}
module CmdArgs (InputOptions(..), processArgs) where

import System.Console.CmdArgs

data InputOptions = Opts
  { output :: FilePath
  , outdir :: FilePath
  , tmpdir :: FilePath
  , input  :: FilePath
  } deriving (Eq,Show,Data,Typeable)

defOutFile = "comicstrip"

defOptions = Opts
  { outdir = "." &= typDir
    &= help "Output directory (default: current directory)"
  , tmpdir = "." &= typDir
    &= help "Temp data directory (default: current directory)"
  , output = defOutFile &= name "o" &= typFile
    &= help ("Name of output file (default: " ++ defOutFile ++ ")")
  , input  = def &= typFile
    &= help "Script to convert"
  }
  &= program "comicbake"
  &= summary "ComicBake v0.0, (c) Dougal Stanton 2010"
  &= help "Convert text scripts into web comics"

processArgs = cmdArgs defOptions
