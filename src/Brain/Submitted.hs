module Brain.Submitted where

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

submittedBrain :: Brain
submittedBrain = toBrain $ do
  mm <- lastOpponentMove
  when (isNothing mm) $ move nop

  -- break
  forever $ do

    reviveOne

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
    reviveOne

    (zombieArgSlot:_) <- exclude [zombieSlot] <$> free
    v <- MonadBrain.vitality opSlot1 opponent
    load v zombieArgSlot
    K `applyCardToField` zombieArgSlot
    zombieSlot `apply` zombieArgSlot
    Put `applyCardToField` zombieArgSlot

    -- alert "before zombie"
    -- break

    -- todo: get their dead slots
    reviveOne

    frees <- free
    frees3 <- exclude [zombieSlot, zombieArgSlot] <$> free
    when (null frees3) $ do
      printState
      alert $ "free: " ++ show frees
      break
    (zombieAttackSlot:_) <- exclude [zombieSlot, zombieArgSlot] <$> free
    zombieAttackSlot `applyFieldToCard` Zombie

    ds <- deadSlots
    -- Attack first
    -- random
    if (null ds)
      then do
        slot <- doAttack
        zombieAttackSlot `applyInt` slot
      else do
        zombieAttackSlot `applyInt` (255 - head ds)
    zombieAttackSlot `apply` zombieSlot
    Put `applyCardToField` zombieSlot

    -- alert "after zombie"
    -- break

exclude :: [SlotNr] -> [SlotNr] -> [SlotNr]
exclude blackList sluts = sluts \\ blackList

reviveOne :: B ()
reviveOne = availableSlot $ \slot -> do
  findSlot (return . dead) $ \j -> do
    slot `applyFieldToCard` Revive
    applyInt slot j

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
  let ss = map snd . sortBy cmpInv . (map . first $ Core.vitality)
             $ filter (\(s,_) -> isI s && alive s) $ zip (V.toList slts) [0 :: Int ..]
  return $ ss

-- | Proponent's slots that are needed for an attack on opponent slot
doAttack :: B SlotNr
doAttack = do
  opSlots <- slots opponent
  let ss = map snd . sortBy cmp . (map . first $ Core.vitality &&& size . Core.field)
             $ zip (V.toList opSlots) [255,254..]
  (s:_) <- return ss
  let opSlot = opSlots V.! (255 - s)
  let v = Core.vitality opSlot
  if v == 0
    then return s
    else if v == 1
         then do
           (p:_) <- free
           p `applyFieldToCard` Dec
           p `applyInt` s
           doAttack
         else do
           (p:q:_) <- free
           attack p s (min v 5000 * 10 `div` 9 + 1) q
           doAttack

isI :: Slot -> Bool
isI (Slot (Card I) _) = True
isI _                 = False

cmpInv :: Ord a => (a, Int) -> (a, Int) -> Ordering
cmpInv (a, i) (b, j)
  | a == b    = intMoves i `compare` intMoves j
  | otherwise = b `compare` a

cmp :: Ord a => (a, Int) -> (a, Int) -> Ordering
cmp (a, i) (b, j)
  | a == b    = intMoves i `compare` intMoves j
  | otherwise = a `compare` b

intMoves :: Int -> Int
intMoves n | n == 0    = 1
           | odd n     = 1 + intMoves (n - 1)
           | otherwise = 1 + intMoves (n `div` 2)