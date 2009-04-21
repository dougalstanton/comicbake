{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Script where

type Character = String
type Frame     = [(Int,Int)] -- box containing character


data Script a = Script
    { scriptTitle    :: String
    , scriptCredits  :: [String]
    , scriptLocation :: FilePath
    , scriptContents :: a
    } deriving (Eq, Show)

instance Functor Script where
    fmap f script = script { scriptContents = f (scriptContents script) }

data Scene = Scene
    { sceneNumber     :: Int
    , sceneBackground :: FilePath
    , sceneAction     :: [Action]
    } deriving (Eq, Show)

data Action = Action
    { character :: Character
    , speech    :: [String]
    , position  :: Maybe Frame
    } deriving (Eq, Show)
