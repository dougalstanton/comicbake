{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Script where

import Data.List (nub)

data Script a = Script { scriptTitle    :: String
                       , scriptCredits  :: [String]
                       , scriptLocation :: FilePath
                       , scriptContents :: a
                       } deriving (Eq, Show)

instance Functor Script where
    fmap f script = script { scriptContents = f (scriptContents script) }

data Scene = Scene
                { sceneNumber       :: Int
                , sceneDescription  :: Maybe String
                , sceneDialogue     :: Dialogue
                , sceneBackground   :: String
                } deriving (Eq, Show)

type Dialogue = [Monologue]
type Monologue = Either Cue Speech
data Cue = Cue { cueActor :: Character
               , cueWords :: [Line]
               , cueCoord :: Maybe [(Int,Int)]
               } deriving (Eq, Show)

type RawScript = String
type Line = String
type Character = String
type Speech = (Character,[Line])

cast = nub . concatMap onstage . scriptContents
    where onstage :: Scene -> [Character]
          onstage = map (either cueActor fst) . sceneDialogue
