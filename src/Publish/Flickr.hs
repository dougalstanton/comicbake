module Publish.Flickr
  (
  -- Take an uploadable item and push it to
  -- the user's Flickr account.
  flickrUpload,
  -- More detailed controls to add/remove
  -- authentication credentials.
  flickrLogin, flickrLogout,
  -- Uploadable item
  UploadData(..)
  ) where

{-

Flickr authentication is specified by the Flickr API
here: http://www.flickr.com/services/api/auth.spec.html

This application has a hard-coded key and secret (see
below) which is used for all interactions. With these
details we log in using by following these steps:

1. Get frob
2. Ask user for permissions
3. Get token

Step 2 happens through a web browser and should only
occur on the first occasion. Thereafter the same token
can be loaded from disk and (provided it's still valid)
reused without further interaction with the user.

The Flickr API also requires that we have a "log out"
procedure, which will be managed by deleting the token.
To log in again the user will have to re-authorise.

-}

import Control.Monad (when)

import Flickr.API
import Flickr.Photos (addTags, getInfo, getPhotoURL, setMeta)
import Flickr.Photos.Upload (uploadPhoto, nullUploadAttr)
import Util.Keys

import System.Directory
import System.IO
import System.FilePath

import Script

-- Upload the file with title, description and tags provided.
-- Returns remote URL of image if uploaded.
flickrUpload :: UploadData -> IO (Maybe URLString)
flickrUpload ul = flickAPI flickr_key (upload ul)

-- Log in to Flickr without uploading an image.
flickrLogin :: IO ()
flickrLogin = flickAPI flickr_key login >> return ()

-- Log out of Flickr. Future uploads require re-authorisation.
flickrLogout :: IO ()
flickrLogout = logout

flickr_key = APIKey
  { apiKind    = "desktop"
  , apiKey     = "201b18127511d29864d1e641c56adaac"
  , apiSecret  = "d336b7258aebe554"
  , apiAuthURL = Nothing -- Just "http://flickr.com/services/auth/?"
  }
flickr_perms = "write"
flickr_attrs = nullUploadAttr
flickr_token_filename = "flickr-token"
flickr_tags = ["comicbake","webcomic"]

instance Show AuthToken where
  show token = "Tok: " ++ authToken token ++ show (authPerms token)

config :: FM a -> FM a
config = withAPIKey flickr_key . withWritePerm

authenticateForDesktop :: String -> FM (URLString, FM AuthToken)
authenticateForDesktop perms = do
  frob <- getFrob
  url  <- mkLoginURL (aFrob frob) perms
  return (url, getToken frob)

loadInBrowser url = do
  putStrLn "Please visit this address to allow ComicBake to upload images:"
  putStrLn url
  putStrLn "When you have authorised ComicBake come back here and continue."
  putStrLn "\nHas ComicBake been authorised? (Y/n)"
  hFlush stdout
  resp <- getLine
  return $ null resp || head resp == 'y' || head resp == 'Y'

loadToken :: IO (Maybe AuthTokenValue)
loadToken = do
  dir <- getAppUserDataDirectory "comicbake"
  let tokenfile = dir </> flickr_token_filename
  Just `fmap` readFile tokenfile
   `catch` (return . const Nothing)

saveToken :: AuthTokenValue -> IO ()
saveToken tok = do
  dir <- getAppUserDataDirectory "comicbake"
  exists <- doesDirectoryExist dir
  when (not exists) (createDirectory dir)
  writeFile (dir </> flickr_token_filename) tok

logout :: IO ()
logout = do
  dir <- getAppUserDataDirectory "comicbake"
  exists <- doesDirectoryExist dir
  if exists
    then removeFile (dir </> flickr_token_filename)
    else return ()

-- Attempt authorisation of application.
login :: FM (Maybe AuthToken)
login = do
  (url, getNewToken) <- authenticateForDesktop flickr_perms
  userAuth <- liftIO $ loadInBrowser url
  if userAuth
    then do token <- getNewToken
            liftIO $ saveToken (authToken token)
            return $ Just token
    else return Nothing

-- Try connecting to Flickr servers. If there is a token on
-- disk we try that first, otherwise we attempt a full login
-- and get the user to authorise in a browser.
connect :: FM (Maybe AuthToken)
connect = do
  mtokval <- liftIO loadToken
  case mtokval of
    Nothing -> login
    Just tv -> do token <- checkToken tv
                  if valid token
                    then liftIO (print token) >> return (Just token)
                    else liftIO invalid >> login
  where invalid = putStrLn "Saved credentials have expired. Reacquiring..."
        valid t = "write" `elem` authPerms t

data UploadData = ULData
  { ulFile  :: FilePath
  , ulInfo  :: Info
  } deriving Show

-- Upload fails if metadata is supplied inline, so we
-- add it separately once the upload is complete.
sendphoto :: UploadData -> FM URLString
sendphoto ul = do
  liftIO $ putStrLn "Trying to upload image..."
  pid <- uploadPhoto (ulFile ul) Nothing Nothing [] flickr_attrs
  addTags pid upload_tags
  setMeta upload_title upload_desc pid
  info <- getInfo pid Nothing
  return (getPhotoURL info)
  where upload_tags = flickr_tags ++ user_tags ul
        upload_title = title (ulInfo ul)
        upload_desc = description (ulInfo ul)
        user_tags = words . map (\c -> if c==',' then ' ' else c) . tags . ulInfo

upload :: UploadData -> FM (Maybe URLString)
upload uldata = do
  mtok <- connect
  case mtok of
    Nothing -> abortmsg >> return Nothing
    Just t  -> authorised t (sendphoto uldata) >>= return . Just
  where abortmsg = liftIO $ putStrLn "Couldn't get authorised. Giving up!"
        authorised t = config . withAuthToken (authToken t)
