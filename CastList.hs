module CastList (CastList, addCharacter, getCoords, emptyCast) where

import qualified Data.Map as M

type Character = String
type Coordinate = (Int, Int)
newtype CastList = ImageMap { imageMap :: (M.Map Character [Coordinate]) }

emptyCast :: CastList
emptyCast = ImageMap M.empty

addCharacter :: Character -> [Coordinate] -> CastList -> CastList
addCharacter ch coords list | validInput = ImageMap $ M.insert ch coords $ imageMap list
                            | otherwise = list
        where validInput = validCharacter ch && validCoords coords

getCoords :: Character -> CastList -> Maybe [Coordinate]
getCoords character = M.lookup character . imageMap

validCharacter = not . null
validCoords = even . length
