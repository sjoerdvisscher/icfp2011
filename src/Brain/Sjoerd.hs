module Brain.Sjoerd where

import Core
import Logic
import Brain

sjoerdBrain :: Brain
sjoerdBrain = brainFromMoves . ($ repeat nop) 
  $ attackAll 255 255

attackAll :: Int -> Int -> [Move] -> [Move]
attackAll (-1) _ = id
attackAll us it = 
    attack us it 5556 123
  . attack (us - 1) it 5556 123
  . attackAll (us - 2) (it - 1)