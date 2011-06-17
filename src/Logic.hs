module Logic where

import Control.Monad.Error
import Control.Monad.State

import Core
import Cards

emptyBoard :: Board
emptyBoard = Board { zombieMode = False, applications = 0, proponent = emptyPlayer, opponent = emptyPlayer }
  where
    emptyPlayer = replicate 255 (Slot (Card I) 10000)

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

applyLeft :: String -> Int -> Result ()
applyLeft s i = do
  let card = read s
  board <- get
  let slots = proponent board
  let app (Slot f v) = Card card `apply` f >>= \f' -> return $ Slot f' v
  slots' <- changeM i app slots
  put $ board { proponent = slots' }

applyRight :: Int -> String -> Result ()
applyRight i s = do
  let card = read s
  board <- get
  let slots = proponent board
  let app (Slot f v) = f `apply` Card card >>= \f' -> return $ Slot f' v
  slots' <- changeM i app slots
  put $ board { proponent = slots' }

