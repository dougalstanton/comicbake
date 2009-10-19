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


go :: [Temperature] -> [Double] -> (a,Energy) -> State a -> a
go (t:ts) (r:rs) (beststate,bestscore)
   st@(State cur energy mknew) | t < epsilon = beststate
                               | otherwise   = st'
  where st' = if delta > 0 || exp(delta/t) > r
                 then go ts rs newbest (update st next)
		 else go (t:ts) rs newbest st
	delta = energy next - energy cur
	next  = mknew r cur
	newbest = if energy next < bestscore
	             then (next,energy next)
		     else (beststate,bestscore)

update :: State a -> a -> State a
update st new = st { state = new }
