module Logic where

import Control.Monad.Error
import Control.Monad.State

import Core
import Cards

emptyBoard :: Board
emptyBoard = Board { zombieMode = False, applications = 0, proponent = emptyPlayer, opponent = emptyPlayer }
  where
    emptyPlayer = replicate 255 (Slot (Card I) 10000)

data ApplyMode = CardToField | FieldToCard
  deriving Show

data Move = Move ApplyMode Int Card
  deriving Show

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
  let app (Slot f v) = do
        f' <- f `apply'` Card card
        return $ Slot f' v
  slots' <- changeM ix app slots
  put $ board { proponent = slots' }

