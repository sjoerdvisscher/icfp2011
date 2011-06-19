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
  reviveAll 0
  attack us it 5556 123
  attack (us - 1) it 5556 123
  attackAll (us - 2) (it - 1)

attackAll1 :: SlotNr -> SlotNr -> B ()
attackAll1 0 _ = return ()
attackAll1 us it = do
  reviveAll 0
  attack us it 3705 123
  attack (us - 1) it 3705 123
  attack (us - 2) it 3705 123
  attackAll1 (us - 3) (it - 1)
  
reviveAll :: Int -> B ()
reviveAll i = do
  v <- vitality i
  f <- field i
  if (v <= 0 || f /= Card I) 
    then reviveAll (i + 1)
    else do
      for_ [0..255] $ \j -> do
      v <- vitality j
      when (v <= 0) $ do
        i `applyFieldToCard` Revive
        applyInt i j

        
        