module Core (

  -- * Vital datatypes
  Board(..), Player, Slot(..), SlotNr,
  Vitality, Field(..), Card(..),

  -- * Vital datatypes
  dead, alive, size,

  -- * Constants
  emptyBoard, initialSlot, deadSlot,

  -- * The Result monad
  Result,

  -- * Helper functions
  showIndexedSlot

  ) where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Vector as V

data Board = Board
  { zombieMode   :: Bool
  , applications :: Int
  , proponent    :: Player
  , opponent     :: Player
  }
  deriving Show

type Player = V.Vector Slot

data Slot = Slot { field :: Field, vitality :: Vitality }

instance Show Slot where
  show (Slot f v) = show f ++ "   {" ++ show v ++ "}"

showIndexedSlot :: Int -> Slot -> String
showIndexedSlot _ (Slot (Card I) 10000) = ""
showIndexedSlot i slot                  = "[" ++ show i ++ "] "
                                          ++ show slot

-- | Index of slot between 0 and 255
type SlotNr = Int

type Vitality = Int

type Result = StateT Board (ErrorT String Identity)

data Field
  = Value Int
  | Card Card
  | Papp1 Card Field
  | Papp2 Card Field Field
  deriving Eq

instance Show Field where
  show (Value x)     = show x
  show (Card c)      = show c
  show (Papp1 c x)   = show c ++ " (" ++ show x ++ ")"
  show (Papp2 c x y) = show c ++ " (" ++ show x ++ ")"
                              ++ " (" ++ show y ++ ")"

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
  deriving Eq

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

dead :: Slot -> Bool
dead (Slot _ x) = x == -1 || x == 0

alive :: Slot -> Bool
alive = not . dead

emptyBoard :: Board
emptyBoard = Board { zombieMode = False, applications = 0, proponent = emptyPlayer, opponent = emptyPlayer }
  where
    emptyPlayer = V.replicate 256 initialSlot

initialSlot :: Slot
initialSlot = Slot (Card I) 10000

deadSlot :: Slot
deadSlot = Slot (Card I) 0

size :: Field -> Int
size (Value _)       = 0
size (Card _)        = 1
size (Papp1 _ f)     = 1 + size f
size (Papp2 _ f1 f2) = 1 + size f1 + size f2

