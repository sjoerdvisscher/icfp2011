module Main where

import Core
import Logic
import Brain

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State

play :: Brain -> Brain -> IO ()
play b1 b2 = do
  putStrLn $ "Player #0"
  (openingMove, brain1) <- playFirst b1
  brain2 <- playSecond b2
  go openingMove emptyBoard 1 brain2 brain1
  where
    go :: Move -> Board -> Int -> NextBrain -> NextBrain -> IO ()
    go move board i curr next = do
      print move
      let Right (rp, board') = runIdentity $ runErrorT $ runStateT (step move) board
      putStrLn $ "Player #" ++ show i
      (move', brain') <- nextMove curr move board'
      putStr rp
      go move' board' (1 - i) next brain'
    step move = do
      mb <- turn move
      s <- report <$> get
      return $ maybe "" (++"\n") mb ++ s

main :: IO ()
main = do
  putStrLn "Lambda: The Gathering, by Magic Missiles"
  play stdinBrain nopBrain

report :: Board -> String
report board = concatMap pr (zip slots [0 :: Int ..])
  where
    slots = opponent board
    pr (Slot (Card I) 10000, _) = ""
    pr tuple                    = show tuple ++ "\n"
