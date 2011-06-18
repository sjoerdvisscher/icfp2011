module Brain.Sjoerd where

import Core
import Logic
import Brain

sjoerdBrain :: Brain
sjoerdBrain = brainFromMoves . ($ repeat nop) 
  $ attackAll 255 255

attackAll :: Int -> Int -> [Move] -> [Move]
attackAll 1 _ = id
attackAll me him = 
    attack me him 5556 123
  . attack (me - 1) him 5556 123
  . attackAll (me - 2) (him - 1)