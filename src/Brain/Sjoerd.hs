module Brain.Sjoerd where

import Core
import Logic
import Brain
import MonadBrain
import BUtils
import Control.Monad (forever)

sjoerdBrain :: Brain
sjoerdBrain = toBrain $ do
  attackAll 255 255
  forever (move nop)

attackAll :: SlotNr -> SlotNr -> B ()
attackAll (-1) it = attackAll1 255 it
attackAll us it = do
  attack us it 5556 123
  attack (us - 1) it 5556 123
  attackAll (us - 2) (it - 1)

attackAll1 :: SlotNr -> SlotNr -> B ()
attackAll1 0 _ = return ()
attackAll1 us it = do
  attack us it 3705 123
  attack (us - 1) it 3705 123
  attack (us - 2) it 3705 123
  attackAll1 (us - 3) (it - 1)
