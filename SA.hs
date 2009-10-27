module SA (Config(..), State(..), anneal)where

type Temperature = Double
type Energy = Double

data Config = Config
  { temperature   :: Temperature
  , coolingfactor :: Double
  , randoms       :: [Double]
  } deriving Show
data State a = State
  { state    :: a
  , value    :: a -> Energy
  , newstate :: Double -> a -> a
  }


epsilon :: Temperature
epsilon = 0.005

-- Carry out simulated annealing, returning the best
-- state we saw through the whole process.
anneal :: Config -> State a -> a
anneal cfg st = go temps (randoms cfg) start st
 where temps = iterate (*(coolingfactor cfg)) (temperature cfg)
       start = (state st, (value st) (state st))


-- The temperatures and the random values should be infinite lists
go :: [Temperature] -> [Double] -> (a,Energy) -> State a -> a
go (t:ts) (r:rs) (bestval,bestscore) oldst@(State cur cost mknew)
                    | t < epsilon = bestval
                    | otherwise   = go newtemps rs newbest newst
  where usenew = delta > 0 || exp(delta/t) > r
	newtemps = if usenew then ts else (t:ts)
	newst = if usenew then update oldst next else oldst
	newbest  = if cost next < bestscore
	                     then (next,cost next)
		             else (bestval,bestscore)

	delta = cost cur - cost next
	next  = mknew r cur
go _ _ _ _ = error "SA failed: reached end of infinite list!"

update :: State a -> a -> State a
update st new = st { state = new }
