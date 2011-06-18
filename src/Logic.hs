module Logic (turn, Move(..), ApplyMode(..), nop) where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Vector as V

import Core
import Cards

data ApplyMode = CardToField | FieldToCard
  deriving Show

data Move = Move ApplyMode Int Card
  deriving Show

-- | Short for @'Move' 'CardToField' 0 'I'@
nop :: Move
nop = Move CardToField 0 I

turn :: Move -> Result (Maybe String)
turn m = do
  preTurn
  r <- (execute m >> return Nothing) `catchError` (return . Just)
  modify (\b -> b { opponent = proponent b, proponent = opponent b, applications = 0 })
  return r

preTurn :: Result ()
preTurn = do
  board <- get
  let slots = proponent board
  modify (\b -> b { zombieMode = True })
  slots' <- V.mapM go slots
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
  let s@(Slot f v) = slots V.! ix
  when (dead s) $ throwError "Slot is dead"
  f' <- f `apply'` Card card
  board' <- get
  slots' <- change ix (const $ Slot f' v) (proponent board')
  put $ board' { proponent = slots' }

