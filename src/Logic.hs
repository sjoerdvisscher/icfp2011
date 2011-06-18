module Logic (turn, Move(..), ApplyMode(..), nop) where

import Control.Monad.Error
import Control.Monad.State

import Core
import Cards

data ApplyMode = CardToField | FieldToCard
  deriving Show

data Move = Move ApplyMode Int Card
  deriving Show

nop :: Move
nop = Move CardToField 0 I

turn :: Move -> Result (Maybe String)
turn m = do
  preTurn
  r <- (execute m >> return Nothing) `catchError` (return . Just)
  modify (\b -> b { opponent = proponent b, proponent = opponent b })
  return r

preTurn :: Result ()
preTurn = do
  board <- get
  let slots = proponent board
  modify (\b -> b { zombieMode = True })
  slots' <- mapM go slots
  modify (\b -> b { zombieMode = False })
  put $ board { proponent = slots' }
  where
    go s = if vitality s == -1
           then do
             try (field s `apply` Card I)
             return (Slot (Card I) 0)
           else return s
    try ma = void ma `catchError` const (return ())

execute :: Move -> Result ()
execute (Move applyMode ix card) = do
  board <- get
  let slots = proponent board
  let apply' =
        case applyMode of
          CardToField -> flip apply
          FieldToCard -> apply
  let app s@(Slot f v) = do
        when (dead s) $ throwError "Slot is dead"
        f' <- f `apply'` Card card
        return $ Slot f' v
  slots' <- changeM ix app slots
  put $ board { proponent = slots' }

