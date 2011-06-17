module Cards where

data Card
  = I
  | Zero
  | Succ
  | Dbl
  | Get
  | Put
  | S
  | K
  | Inc
  | Dec
  | Attack
  | Help
  | Copy
  | Revive
  | Zombie

instance Show Card where
  show I      = "I"
  show Zero   = "zero"
  show Succ   = "succ"
  show Dbl    = "dbl"
  show Get    = "get"
  show Put    = "put"
  show S      = "S"
  show K      = "K"
  show Inc    = "inc"
  show Dec    = "dec"
  show Attack = "attack"
  show Help   = "help"
  show Copy   = "copy"
  show Revive = "revive"
  show Zombie = "zombie"

instance Read Card where
  readsPrec _ = f
    where
      f "I"      = [(I, "")]
      f "zero"   = [(Zero, "")]
      f "succ"   = [(Succ, "")]
      f "dbl"    = [(Dbl, "")]
      f "get"    = [(Get, "")]
      f "put"    = [(Put, "")]
      f "S"      = [(S, "")]
      f "K"      = [(K, "")]
      f "inc"    = [(Inc, "")]
      f "dec"    = [(Dec, "")]
      f "attack" = [(Attack, "")]
      f "help"   = [(Help, "")]
      f "copy"   = [(Copy, "")]
      f "revive" = [(Revive, "")]
      f "zombie" = [(Zombie, "")]
      f _        = []

