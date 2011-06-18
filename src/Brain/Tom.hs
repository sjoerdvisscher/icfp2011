module Brain.Tom where

import Core
import Logic
import Brain

-- [3] 5
-- [6] inc
-- goal: [8] 6

-- heap: [100..]


tomBrain :: Brain
tomBrain = brainFromMoves
  $ cycle
  $ setup 
  . copy' 6 100 
  . apply 100 3 
  . copy' 100 8
  $ []
  where
    setup = load' 5 3 . (Move FieldToCard 6 Succ :)

