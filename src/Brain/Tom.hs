module Brain.Tom where

import Core
import Logic
import Brain
import MonadBrain
import BUtils

import Control.Arrow
import Control.Applicative
import Control.Monad (forever, when)
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import Prelude hiding (break)

tomBrain :: Brain
tomBrain = toBrain $ do
  mm <- lastOpponentMove
  when (isNothing mm) $ move nop

  -- Attack first
  -- random
  let attackSlot = 124
  attack attackSlot 0 5556 123
  attack attackSlot 0 5556 123

  -- break
  forever $ do

    (zombieSlot:_) <- free
    zombieSlot `applyFieldToCard` Help
    (opSlot1:_) <- interesting
    zombieSlot `applyInt` opSlot1
    (opSlot2:_) <- (\\ [opSlot1]) <$> interesting
    zombieSlot `applyInt` opSlot2
    K `applyCardToField` zombieSlot
    S `applyCardToField` zombieSlot

    (zombieArgSlot:_) <- (\\ [zombieSlot]) <$> free
    v <- MonadBrain.vitality opSlot1 opponent
    load v zombieArgSlot
    K `applyCardToField` zombieArgSlot
    zombieSlot `apply` zombieArgSlot

    -- alert "before zombie"
    -- break

    -- todo: get their dead slots
    (zombieAttackSlot:_) <- (\\ [zombieSlot, zombieArgSlot]) <$> free
    zombieAttackSlot `applyFieldToCard` Zombie
    (opSlot3:_) <- deadSlots
    zombieAttackSlot `applyInt` opSlot3
    zombieAttackSlot `apply` zombieSlot

    -- alert "after zombie"
    -- break

-- | Opponent's slots that are alive, sorted by size
interesting :: B [SlotNr]
interesting = do
  slts <- slots opponent
   -- shuffle
  let ss = map snd . reverse . sort . (map . first $ size . Core.field)
             $ zip (filter alive $ V.toList slts) [0 :: Int ..]
  return $ ss ++ [0..] -- In case there's a bug, return everything

-- | Dead's slots that are dead, with the biggest expresssion
deadSlots :: B [SlotNr]
deadSlots = do
  slts <- slots proponent
  let ss = map snd . reverse . sort . (map . first $ size . Core.field)
             $ zip (filter dead $ V.toList slts) [0 :: Int ..]
  return $ ss

-- | Proponent's slots that are empty, sorted by vitality
free :: B [SlotNr]
free = do
  slts <- slots proponent
  let ss = map snd . reverse . sort . (map . first $ Core.vitality)
             $ zip (filter (\s -> isI s && alive s)
                           $ V.toList slts) [0 :: Int ..]
  return $ ss
  where
    isI :: Slot -> Bool
    isI (Slot (Card I) _) = True
    isI _                 = False

