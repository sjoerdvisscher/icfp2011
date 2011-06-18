module Brain.Mirror where

import Logic
import Brain


mirrorBrain :: Brain
mirrorBrain = Brain
  { playFirst  = return (nop, nextBrain) 
  , playSecond = return nextBrain
  }
  where
    nextBrain = NextBrain (\m _ -> return (m, nextBrain))

