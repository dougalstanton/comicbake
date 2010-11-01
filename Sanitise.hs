module Sanitise (Panel(..), sanitiseScene) where

import Script
import Parse

data Panel a = Panel
    { number     :: Int
    , background :: FilePath
    , bgsize     :: Maybe Dim
    , action     :: a
    } deriving Show

instance Functor Panel where
 fmap f p = p { action = f (action p) }

sanitiseScene :: Scene -> Panel [Action]
sanitiseScene sc = Panel
    { number = sceneNumber sc
    , background = sceneBackground sc
    , bgsize = Nothing
    , action = sceneAction sc }
