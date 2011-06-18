module Brain.Sjoerd where

import Core
import Logic
import Brain

sjoerdBrain :: Brain
sjoerdBrain = brainFromMoves . ($ repeat nop) 
  $ attackAll 255 255

attackAll :: SlotNr -> SlotNr -> [Move] -> [Move]
attackAll (-1) it = attackAll1 255 it
attackAll us it = 
    attack us it 5556 123
  . attack (us - 1) it 5556 123
  . attackAll (us - 2) (it - 1)

attackAll1 :: SlotNr -> SlotNr -> [Move] -> [Move]
attackAll1 0 _ = id
attackAll1 us it = 
    attack us it 3705 123
  . attack (us - 1) it 3705 123
  . attack (us - 2) it 3705 123
  . attackAll1 (us - 3) (it - 1)
