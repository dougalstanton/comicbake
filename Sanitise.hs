module Sanitise (Pane(..), sanitiseScene) where

import Script
import Parse

data Pane a = Pane { number     :: Int
                   , background :: FilePath
                   , bgsize     :: Maybe Dim
                   , action     :: a
                   }

instance Functor Pane where
 fmap f p = p { action = f (action p) }

sanitiseScene :: Scene -> Pane [Action]
sanitiseScene sc = Pane { number = sceneNumber sc
                        , background = sceneBackground sc
                        , bgsize = Nothing
                        , action = sceneAction sc }
