module ImageMapParse (parseImageMap) where

import Text.HTML.TagSoup

import CastList

parseImageMap :: String -> CastList
parseImageMap = foldr (uncurry addCharacter) emptyCast . map getData . keepAreaTag . parseTags
    where keepAreaTag = filter (isTagOpenName "area")
          getData t = (fromAttrib "href" t, mkCoordPairs $ readCoords $ fromAttrib "coords" t)
          readCoords str = read ('[':str ++ "]")
          mkCoordPairs :: [Int] -> [(Int,Int)]
          mkCoordPairs [] = []
          mkCoordPairs [a] = []
          mkCoordPairs (a:b:ls) = (a,b):mkCoordPairs ls
