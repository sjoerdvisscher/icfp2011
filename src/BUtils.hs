module BUtils (

  applyCardToField, applyFieldToCard,
  fromI, findSlot, availableSlot,
  load, copy, apply, applyInt, composeCard, attack

  ) where

import MonadBrain
import Core hiding (vitality, field)
import Logic

applyCardToField :: Card -> SlotNr -> B ()
applyCardToField card slot = move (Move CardToField slot card)

applyFieldToCard :: SlotNr -> Card -> B ()
applyFieldToCard slot card = move (Move FieldToCard slot card)

-- | Make sure the specified slot contains 'I' before executing the action.
fromI :: (SlotNr -> B a) -> SlotNr -> B a
fromI f i = do
  contents <- field i
  case contents of
    Card I -> return ()
    _      -> move (Move CardToField i Put)
  f i

-- | Find a slot which matches the given predicate, and pass its slot number to the second argument.
findSlot :: (Slot -> B Bool) -> (SlotNr -> B ()) -> B ()
findSlot p f = go 255 where
  go (-1) = return ()
  go i = do
    s <- slotAt i
    b <- p s
    if b then f i else go (i - 1)
    
-- | Find a slot that is empty and alive.
availableSlot :: (SlotNr -> B ()) -> B ()
availableSlot = findSlot (\(Slot f v) -> return $ v > 0 && f == Card I)
    
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
  applyFieldToCard slot card

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
atVital f slot = go 0 where
  go 256 = f slot
  go n = let slot' = (slot + n) `mod` 255 in do
    v <- vitality slot' proponent
    if (v < 100) then go (n + 1) else f slot'
