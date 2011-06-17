module Slots where

import Cards

data Slot = Slot Field Vitality
  deriving Show

type Vitality = Int

data Field
  = Value Int
  | Function Card
  deriving Show


slotNr :: Slot -> Maybe Int
slotNr (Slot (Value x) _)
  | 0 <= x && x <= 255    = Just x
slotNr _                  = Nothing

dead :: Slot -> Bool
dead (Slot _ x)
  | x == -1 || x == 0 = True
dead _                = False

alive :: Slot -> Bool
alive = not . dead

