module CastList where

import qualified Data.Map as M

type Character = String
type Coordinate = (Int, Int)
type CastList = M.Map Character [Coordinate]

addCharacter :: Character -> [Coordinate] -> CastList -> CastList
addCharacter ch coords list | validInput = M.insert ch coords list
                            | otherwise = list
        where validInput = validCharacter ch && validCoords coords

validCharacter = not . null
validCoords = even . length
