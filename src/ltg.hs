module Main where

import Core
import Cards
import Logic
import Brain

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Data.Maybe

main = do
  putStrLn "Lambda: The Gathering, by Simons Fanboys"
  go emptyBoard 0
  where
    go board i = do
      putStrLn $ "Player #" ++ show i
      move <- playerBrain board
      case runIdentity $ runErrorT $ runStateT (step move) board of
        Right (rp, s') -> putStrLn rp >> go s' (1 - i)
        Left err       -> putStrLn err
    step move = do
      mb <- turn move
      s <- report <$> get
      return $ maybe "" (++"\n") mb ++ s

report :: Board -> String
report board = concatMap pr (zip slots [0..])
  where
    slots = opponent board
    pr (Slot (Card I) 10000, _) = ""
    pr tuple                    = show tuple ++ "\n"
