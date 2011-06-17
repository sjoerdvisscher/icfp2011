module Logic where

import Control.Monad.State

import Core
import Cards

emptyBoard :: Board
emptyBoard = Board 0 emptyPlayer emptyPlayer
  where
    emptyPlayer = replicate 255 (Slot (Card I) 10000)


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

