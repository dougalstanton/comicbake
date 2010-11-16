module CastList (CastList, addCharacter, getCoords, emptyCast) where

import qualified Data.Map as M
import qualified Data.Char as C

type Character = String
type Coordinate = (Int, Int)
newtype CastList = ImageMap { imageMap :: (M.Map Character [Coordinate]) }

instance Show CastList where
    show = show . imageMap

emptyCast :: CastList
emptyCast = ImageMap M.empty

addCharacter :: Character -> [Coordinate] -> CastList -> CastList
addCharacter ch coords list | validInput = ImageMap $ M.insert ch coords $ imageMap list
                            | otherwise = list
        where validInput = validCharacter ch && validCoords coords

getCoords :: Character -> CastList -> Maybe [Coordinate]
getCoords char = M.lookup character . imageMap
  where character = map C.toLower char

validCharacter = not . null
validCoords = even . length
