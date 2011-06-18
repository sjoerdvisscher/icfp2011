module Main where

import Core
import Logic
import Brain
import Brain.Loop
import Brain.Mirror
import Brain.Nop
import Brain.NopReturn
import Brain.Stdin
import Brain.Sjoerd

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import System.Environment
import System.IO

play :: Brain -> Brain -> Bool -> Bool -> IO ()
play b1 b2 first debug = do
  when (debug) $ hPutStrLn stderr $ "Player #0   [0]"
  (openingMove, brain1) <- playFirst b1
  brain2 <- playSecond b2
  go openingMove emptyBoard 1 brain2 brain1
  where
    go :: Move -> Board -> Int -> NextBrain -> NextBrain -> IO ()
    go move board ply curr next = do
      when (debug) $ hPrint stderr move
      when (first == (ply `rem` 2 /= 0)) $ writeMove move
      let Right (rp, board') = runIdentity $ runErrorT $ runStateT (step move) board
      if ply == 200000
        then do
             when (debug) $ hPutStrLn stderr "Game done after 100000 moves each."
             when (debug) $ hPutStrLn stderr $ "Player #" ++ show (ply `rem` 2) ++ " has: " ++ show (score $ proponent board')
             when (debug) $ hPutStrLn stderr $ "Player #" ++ show (1 - (ply `rem` 2)) ++ " has: " ++ show (score $ opponent board)
        else if all dead (opponent board)
             then when (debug) $ hPutStrLn stderr $ "Player #" ++ show (ply `rem` 2) ++ " won!"
             else if all dead (proponent board)
                  then when (debug) $ hPutStrLn stderr $ "Player #" ++ show (1 - (ply `rem` 2)) ++ " won!"
                  else do
                       when (debug) $ hPutStr stderr rp
                       when (debug) $ hPutStrLn stderr $ "Player #" ++ show (ply `rem` 2) ++ "   [" ++ show (ply `quot` 2) ++ "]"
                       (move', brain') <- nextMove curr move board'
                       go move' board' (succ ply) next brain'
    step move = do
      mb <- turn move
      s <- report <$> get
      return $ maybe "" (++"\n") mb ++ s
    score :: Player -> Int
    score = length . filter alive

main :: IO ()
main = do
  hPutStrLn stderr "Lambda: The Gathering, by Magic Missiles"
  args <- getArgs
  case map (`eitherLookup` brains) args of
    []                   -> play stdinBrain  stdinBrain  True  True
    [Left "0"]           -> play sjoerdBrain stdinBrain  True  False
    [Left "1"]           -> play stdinBrain  sjoerdBrain False False
    [Right b1, Right b2] -> play b1          b2          True  True
    _                    -> do
      hPutStrLn stderr "Usage: ltg <brain> <brain>"
      hPutStrLn stderr $ "  where  brain `elem` " ++ show (map fst brains) 
  where
    eitherLookup e as = maybe (Left e) Right $ lookup e as

brains :: [(String, Brain)]
brains =
  [ ("nop",    nopBrain)
  , ("nopret", nopReturnBrain)
  , ("stdin",  stdinBrain)
  , ("mirror", mirrorBrain)
  , ("loop",   loopBrain)
  , ("sjoerd", sjoerdBrain)
  ]

report :: Board -> String
report board = concatMap pr (zip slots [0 :: Int ..])
  where
    slots = opponent board
    pr (Slot (Card I) 10000, _) = ""
    pr tuple                    = show tuple ++ "\n"

writeMove :: Move -> IO ()
writeMove (Move CardToField i c) = do
  putStrLn "1"
  print c
  print i
  hFlush stdout
writeMove (Move FieldToCard i c) = do
  putStrLn "2"
  print i
  print c
  hFlush stdout
