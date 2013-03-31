module CmdArgs ( execParser, options
               , Modes(..), Build(..), Publish(..)) where

import Options.Applicative

data Build = Build
           { output :: FilePath
           , outdir :: FilePath
           , tmpdir :: FilePath
           , input  :: FilePath
           } deriving Show

build :: Parser Build
build = Build
    <$> strOption
        (long "output" <> short 'o'
        <> value "strip" <> metavar "FILE"
        <> help "Output comic to FILE.png")
    <*> strOption
        (long "dir" <> short 'd'
        <> value "." <> help "Path to save output comic file")
    <*> strOption
        (long "tmpdir" <> short 't'
        <> value "." <> help "Path to save intermediate files")
    <*> argument str (metavar "SCRIPT")

data Publish = Publish
             { cli_title     :: Maybe String
             , cli_desc      :: Maybe String
             , cli_tags      :: [String]
             , enableFlickr  :: Bool
             , disableFlickr :: Bool
             , comicstrip    :: FilePath
             } deriving Show

-- cheap and cheerful, doesn't deal with escaped commas
commalist = return . words . map (\c -> if c==',' then ' ' else c)

publish :: Parser Publish
publish = Publish
      <$> optional (strOption (long "title"))
      <*> optional (strOption (long "description"))
      <*> nullOption (long "tags" <> help "Comma-delimited list"
                                <> reader commalist <> value [])
      <*> switch (long "enableFlickr" <> help "Enable without uploading")
      <*> switch (long "disableFlckr" <> help "Force disable Flickr")
      <*> argument str (metavar "PATH")


data Modes = MkComic Build | UploadComic Publish
             deriving Show

modes :: Parser Modes
modes = subparser
          (command "build"
                   (info (helper <*> buildmode)
                         (progDesc "Generate a comic!"))
        <> command "publish"
                   (info (helper <*> uploadmode)
                                   (progDesc "Upload a comic!")))
  where buildmode = MkComic <$> build
        uploadmode = UploadComic <$> publish

options = info (helper <*> modes)
               ( fullDesc
               <> progDesc "Convert text scripts into web comics"
               <> header "ComicBake v0.2, (c) Dougal Stanton 2010-11"
               <> footer "Pass --help to a subcommand for more details.")

