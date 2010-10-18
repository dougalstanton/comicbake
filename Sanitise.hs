module Sanitise (Pane(..), sanitiseScene) where

import Parse

data Pane a = Pane { number     :: Int
                   , background :: FilePath
                   , action     :: a
                   }

instance Functor Pane where
 fmap f p = p { action = f (action p) }

sanitiseScene :: Scene -> Pane [Action]
sanitiseScene sc = Pane { number = sceneNumber sc
                        , background = sceneBackground sc
                        , action = sceneAction sc }
