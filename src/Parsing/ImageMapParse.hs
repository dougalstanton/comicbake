module Parsing.ImageMapParse (parseImageMap) where

import Text.HTML.TagSoup

import CastList

parseImageMap :: String -> CastList
parseImageMap = foldr (uncurry addCharacter) emptyCast . map getData . keepAreaTag . parseTags
    where keepAreaTag = filter (isTagOpenName "area")
          getData t = (fromAttrib "href" t, mkCoordPairs $ readCoords $ fromAttrib "coords" t)
          readCoords = map read . words . map (\c -> if c==',' then ' ' else c)
          mkCoordPairs :: [Int] -> [(Int,Int)]
          mkCoordPairs (a:b:ls) = (a,b):mkCoordPairs ls
          mkCoordPairs _        = []
