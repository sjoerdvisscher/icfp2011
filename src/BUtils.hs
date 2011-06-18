module BUtils (load, copy, apply, applyInt, composeCard, attack) where

import MonadBrain
import Core
import Logic

-- | Loads Int into a Slot
load :: Int -> SlotNr -> B ()
load n = fromI $ \slot ->
  case n of
    0             -> move (Move FieldToCard slot Zero)
    _ | odd n     -> load (n - 1)     slot >> move (Move CardToField slot Succ)
    _ | otherwise -> load (n `div` 2) slot >> move (Move CardToField slot Dbl)

-- | Copies field of src to field of dest
copy :: SlotNr -> SlotNr -> B ()
copy src dest = do
  load src dest
  move (Move CardToField dest Get)

-- | Applies function in slot `func` to value in slot `arg`, replacing slot with
-- value
apply :: SlotNr -> SlotNr -> B ()
apply func arg = do
  composeCard func Get
  applyInt func arg
  move (Move FieldToCard func Zero)

-- | Applies function in slot to value x, replacing slot with value
applyInt :: SlotNr -> Int -> B ()
applyInt slot 0 = move (Move FieldToCard slot Zero)
applyInt slot n
  | odd n     = composeCard slot Succ >> applyInt slot (n - 1)    
  | otherwise = composeCard slot Dbl  >> applyInt slot (n `div` 2)

-- | Composes function in slot with card, replacing field slot with the
-- composition
composeCard :: SlotNr -> Card -> B ()
composeCard slot card = do
  move (Move CardToField slot K)
  move (Move CardToField slot S)
  move (Move FieldToCard slot card)

-- | Attack!
attack :: SlotNr -> SlotNr -> Int -> SlotNr -> B ()
attack i j n slot = do
  move (Move CardToField slot Put)
  move (Move FieldToCard slot Attack)
  applyInt slot i
  applyInt slot j
  applyInt slot n
