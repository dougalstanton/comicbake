module Graphics.Pix where

import Control.Monad
import Graphics.GD
import System.FilePath

import Script
import Comic.Locations
import Comic.Layout
import Graphics.Drawing

-- Write speech to the image, using the given offset to account
-- for extra margine being added to the image since the speech
-- location was calculated.
addspeech :: Int -> Image -> Speech -> IO Image
addspeech off img (Speech (Loc txtbox txt) (Loc speakerbox _)) = do
  speechbubble txt (off += midpoint txtbox) (off += midpoint speakerbox) img
  return img
  where (+=) v (x,y) = (v+x,v+y)

writeImage :: FilePath -> FilePath -> FilePath -> Panel [Speech] -> IO (Panel FilePath)
writeImage srcdir destdir filename panel = do
  img <- expandImage =<< loadPngFile (srcdir </> (background panel))
  _ <- foldM (addspeech 10) img (action panel)
  let imgfile = destdir </> filename <.> show (number panel) <.> "png"
  savePngFile imgfile img
  newsize <- imageSize img
  return (panel {bgsize = Just newsize, action = imgfile})

-- Return an image which contains the original surrounded by
-- a black margin (equal size on all sides).
expandImage :: Image -> IO Image
expandImage src = do
  (width,height) <- imageSize src
  dst <- newImage (width+20,height+20)
  fillImage (rgb 0 0 0) dst
  copyRegion (0,0) (width,height) src (10,10) dst
  return dst

writeHeader = writeBookend header "head"
writeFooter = writeBookend footer "foot"

writeBookend writer role txt width destdir filename = do
  img <- newImage (width,30)
  fillImage (rgb 0 0 0) img
  _ <- writer txt img
  let imgfile = destdir </> filename <.> role <.> "png"
  savePngFile imgfile img
  return imgfile
