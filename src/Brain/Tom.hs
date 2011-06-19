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
import Data.List (sort, (\\), sortBy)
import qualified Data.Vector as V
import Prelude hiding (break)
import Data.Ord

tomBrain :: Brain
tomBrain = toBrain $ do
  mm <- lastOpponentMove
  when (isNothing mm) $ move nop

  -- Attack first
  -- random
  attack 128 0 5556 123
  attack 129 0 5556 123

  -- break
  forever $ do

    (zombieSlot:_) <- free
    zombieSlot `applyFieldToCard` Help
    (opSlot1:_) <- interesting
    zombieSlot `applyInt` opSlot1
    (opSlot2:_) <- exclude [opSlot1] <$> interesting
    zombieSlot `applyInt` opSlot2
    K `applyCardToField` zombieSlot
    S `applyCardToField` zombieSlot

    --printState
    --break
    (zombieArgSlot:_) <- exclude [zombieSlot] <$> free
    v <- MonadBrain.vitality opSlot1 opponent
    load v zombieArgSlot
    K `applyCardToField` zombieArgSlot
    zombieSlot `apply` zombieArgSlot
    Put `applyCardToField` zombieArgSlot

    -- alert "before zombie"
    -- break

    -- todo: get their dead slots
    frees <- free
    --printState
    --alert $ "free: " ++ show frees
    -- break
    (zombieAttackSlot:_) <- exclude [zombieSlot, zombieArgSlot] <$> free
    zombieAttackSlot `applyFieldToCard` Zombie
    (opSlot3:_) <- deadSlots
    zombieAttackSlot `applyInt` (255 - opSlot3)
    zombieAttackSlot `apply` zombieSlot
    Put `applyCardToField` zombieSlot

    -- alert "after zombie"
    -- break

exclude :: [SlotNr] -> [SlotNr] -> [SlotNr]
exclude blackList sluts = sluts \\ blackList

-- | Opponent's slots that are alive, sorted by size
interesting :: B [SlotNr]
interesting = do
  slts <- slots opponent
  let ss = map snd . reverse . sortBy (comparing fst) . reverse . (map . first $ size . Core.field)
             $ filter (alive . fst) $ zip (V.toList slts) [0 :: Int ..]
  return $ ss ++ [0..255] -- In case there's a bug, return everything

-- | Dead's slots that are dead, with the biggest expresssion
deadSlots :: B [SlotNr]
deadSlots = do
  slts <- slots opponent
  let ss = map snd . reverse . sort . (map . first $ size . Core.field)
             $ filter (dead . fst) $ zip (V.toList slts) [0 :: Int ..]
  return $ ss

-- | Proponent's slots that are empty, sorted by vitality
free :: B [SlotNr]
free = do
  slts <- slots proponent
  let ss = map snd . reverse . sortBy (comparing fst) . reverse . (map . first $ Core.vitality)
             $ filter (\(s,_) -> isI s && alive s) $ zip (V.toList slts) [0 :: Int ..]
  return $ ss
  where
    isI :: Slot -> Bool
    isI (Slot (Card I) _) = True
    isI _                 = False

