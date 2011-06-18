module BUtils (

  applyCardToField, applyFieldToCard,
  load, copy, apply, applyInt, composeCard, attack

  ) where

import MonadBrain
import Core hiding (vitality)
import Logic

applyCardToField :: Card -> SlotNr -> B ()
applyCardToField card slot = move (Move CardToField slot card)

applyFieldToCard :: SlotNr -> Card -> B ()
applyFieldToCard slot card = move (Move FieldToCard slot card)

-- | Loads Int into a Slot
load :: Int -> SlotNr -> B ()
load n = fromI $ \slot ->
  case n of
    0             -> applyFieldToCard slot Zero
    _ | odd n     -> load (n - 1)     slot >> applyCardToField Succ slot
    _ | otherwise -> load (n `div` 2) slot >> applyCardToField Dbl slot

-- | Copies field of src to field of dest
copy :: SlotNr -> SlotNr -> B ()
copy src dest = do
  load src dest
  applyCardToField Get dest

-- | Applies function in slot `func` to value in slot `arg`, replacing slot with
-- value
apply :: SlotNr -> SlotNr -> B ()
apply func arg = do
  composeCard func Get
  applyInt func arg
  applyFieldToCard func Zero

-- | Applies function in slot to value x, replacing slot with value
applyInt :: SlotNr -> Int -> B ()
applyInt slot 0 = applyFieldToCard slot Zero
applyInt slot n
  | odd n     = composeCard slot Succ >> applyInt slot (n - 1)    
  | otherwise = composeCard slot Dbl  >> applyInt slot (n `div` 2)

-- | Composes function in slot with card, replacing field slot with the
-- composition
composeCard :: SlotNr -> Card -> B ()
composeCard slot card = do
  applyCardToField K slot
  applyCardToField S slot
  applyCardToField card slot

-- | Attack!
attack :: SlotNr -> SlotNr -> Int -> SlotNr -> B ()
attack i j n slotInit = atVital attack' slotInit
  where
    attack' = fromI $ \slot -> do
      applyFieldToCard slot Attack
      applyInt slot i
      applyInt slot j
      applyInt slot n

-- | Run a function at a vital slot, given a start slot.
atVital :: (SlotNr -> B a) -> SlotNr -> B a
atVital f 256 = atVital f 0
atVital f slot = do
  v <- vitality slot
  if (v < 100) then atVital f (slot + 1) else f slot
