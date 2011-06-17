module Apply where

import Control.Monad.State

import Board
import Cards

type Result = Either String Int

type SlotNr = Int

data CurrentPlayer = Player0 | Player1
  deriving Show

type Proponent = Player
type Opponent  = Player

leftApply :: Card -> SlotNr -> State (Proponent, Opponent) Result
leftApply I x = return $ Right x
leftApply _ _ = return $ Left "not implemented"

