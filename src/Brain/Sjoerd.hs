module Brain.Sjoerd where

import Core hiding (vitality, field)
import Logic
import Brain
import MonadBrain
import BUtils
import Control.Monad (forever, when)
import Data.Foldable

sjoerdBrain :: Brain
sjoerdBrain = toBrain $ do
  attackAll 255 255
  forever (move nop)

attackAll :: SlotNr -> SlotNr -> B ()
attackAll (-1) it = attackAll1 255 it
attackAll us it = do
  reviveOne
  attack us it 5556 123
  attack (us - 1) it 5556 123
  attackAll (us - 2) (it - 1)

attackAll1 :: SlotNr -> SlotNr -> B ()
attackAll1 0 _ = return ()
attackAll1 us it = do
  reviveOne
  attack us it 3705 123
  attack (us - 1) it 3705 123
  attack (us - 2) it 3705 123
  attackAll1 (us - 3) (it - 1)
  
reviveAll :: B ()
reviveAll = availableSlot $ \slot -> do
  for_ [0..255] $ \j -> do
    v <- vitality j proponent
    when (v <= 0) $ do
      slot `applyFieldToCard` Revive
      applyInt slot j

reviveOne :: B ()
reviveOne = availableSlot $ \slot -> do
  findSlot (return . dead) $ \j -> do
    slot `applyFieldToCard` Revive
    applyInt slot j
        
