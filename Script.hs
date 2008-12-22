{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Script where

import Data.List (nub)

data Script a = Script { scriptTitle    :: String
                       , scriptCredits  :: [String]
                       , scriptContents :: a
                       } deriving (Eq, Show)

instance Functor Script where
    fmap f script = script { scriptContents = f (scriptContents script) }

data Scene = Scene { dialogue    :: [Speech] 
                   , description :: [String]
                   , background  :: FilePath } deriving (Eq, Show)

type Line = String
type Character = String
type Speech = (Character,[Line])
