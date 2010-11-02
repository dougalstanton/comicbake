module Geometry where

import Script

overlaps b1 b2 = overlapX b1 b2 && overlapY b1 b2
overlapRange (a1,a2) (b1,b2) = straddle || inside
  where straddle = b1 < a1 && a1 < b2 || b1 < a2 && a2 < b2
        inside = a1 <= b1 && b2 <= a2 || b1 <= a1 && a2 <= b2
overlapX (Box (a1,_) (a2,_)) (Box (b1,_) (b2,_)) = overlapRange (a1,a2) (b1,b2)
overlapY (Box (_,a1) (_,a2)) (Box (_,b1) (_,b2)) = overlapRange (a1,a2) (b1,b2)
