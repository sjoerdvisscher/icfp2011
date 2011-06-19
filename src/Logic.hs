module Logic (turn, Move(..), ApplyMode(..), nop) where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.Vector as V

import Core
import Cards

data ApplyMode = CardToField | FieldToCard
  deriving Show

data Move = Move ApplyMode SlotNr Card
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
  modify (\b -> b { zombieMode = True })
  forM_ [0..255] go
  modify (\b -> b { zombieMode = False })
  where
    go i = do
           board <- get
           let slots = proponent board
               slot  = slots V.! i
           when (vitality slot == -1) $ do
             try (field slot `apply` Card I)
             board' <- get
             let slots' = proponent board'
             slots'' <- change i (const deadSlot) slots'
             modify (\b -> b { applications = 0, proponent = slots'' })
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

