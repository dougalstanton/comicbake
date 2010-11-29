module Locations where

import Script
import Objects
import CastList

-- Bring together the character names and their locations
-- in the first stage of "pinning" items to each panel.
--
-- Once each character is placed we can start grouping
-- speech bubbles around them, making sure to avoid out of
-- order speeches and awkward overlapping.

type FixedPerson = Located Person

data Located a = Loc { location :: Box
                     , item     :: a } deriving (Eq,Show)

instance IsFrame (Located a) where
  dim (Loc box _) = dim box
  coords (Loc box _) = coords box

-- Cast lists are stored with lists of coordinates
coords2box [first,second] = Box first second
coords2box _ = error "Locations:coords2box can't handle coord lists /= 2"

-- Bring together person with the cast list and pull out this
-- person's location, if it exists.
findPersonLocation :: Person -> CastList -> Box
findPersonLocation person castlist = maybe errmsg coords2box mebbebox
  where mebbebox = getCoords (unperson person) castlist
        errmsg = error "Locations:findPersonLocation has unknown person"

-- Add a location to each person.
addCharCoords :: CastList -> [Person] -> [FixedPerson]
addCharCoords castlist ppl =  map f ppl
  where f person = Loc (findPersonLocation person castlist) person


-- Attributed text has a named speaker but no location itself.
data AttributedText = AttrBubble [String] FixedPerson deriving (Eq,Show)

getFixedPerson :: Character -> [FixedPerson] -> FixedPerson
getFixedPerson ch ppl = maybe errmsg id $ lookup ch $ map munge ppl
  where munge l@(Loc _ p) = (unperson p, l)
        errmsg = error "Locations:getFixedPerson can't locate person for speech"

attributeText :: [FixedPerson] -> Text -> AttributedText
attributeText ppl (Bubble str ch) = AttrBubble str (getFixedPerson ch ppl)


-- Convert a collection of free-floating objects into a list
-- of attributed speech bubbles with known locations.
convert :: CastList -> Objects -> [AttributedText]
convert castlist (Objs txt ppl) = map (attributeText newppl) txt
  where newppl = addCharCoords castlist ppl
