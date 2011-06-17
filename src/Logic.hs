module Logic where

import Control.Monad.State

import Core
import Cards

emptyBoard :: Board
emptyBoard = Board 0 emptyPlayer emptyPlayer
  where
    emptyPlayer = replicate 255 (Slot (Card I) 10000)

data ApplyMode = CardToField | FieldToCard
  deriving Show

data Move = Move ApplyMode Int Card
  deriving Show

execute :: Move -> Result ()
execute (Move applyMode ix card) = do
  board <- get
  let slots = proponent board
  let apply' =
        case applyMode of
          CardToField -> flip apply
          FieldToCard -> apply
  let app (Slot f v) = do
        f' <- f `apply'` Card card
        return $ Slot f' v
  slots' <- changeM ix app slots
  put $ board { proponent = slots' }
