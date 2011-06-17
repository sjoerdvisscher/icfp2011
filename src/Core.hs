module Core where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

data Board = Board
  { applications :: Int
  , proponent :: Player
  , opponent :: Player
  }
  deriving Show

type Player = [Slot]

data Slot = Slot { field :: Field, vitality :: Vitality }
  deriving Show

type Vitality = Int

type Result = StateT Board (ErrorT String Identity)

data Field
  = Value Int
  | Function (Field -> Result Field)

instance Show Field where
  show (Value x)    = "Value " ++ show x
  show (Function _) = "Function _"

getValue :: Field -> Result Int
getValue (Value x) = return $ x
getValue _         = throwError "Field is not a value"

apply :: Field -> Field -> Result Field
apply (Function f) x = do
                       board <- get
                       let apps = applications board + 1
                       put $ board { applications = apps }
                       when (apps == 1000)
                         $ throwError "it's been a 1000 applications"
                       f x
apply _            _ = throwError "Field is not a function"

slotNr :: Slot -> Maybe Int
slotNr (Slot (Value x) _)
  | 0 <= x && x <= 255    = Just x
slotNr _                  = Nothing

dead :: Slot -> Bool
dead (Slot _ x) = x == -1 || x == 0

alive :: Slot -> Bool
alive = not . dead

