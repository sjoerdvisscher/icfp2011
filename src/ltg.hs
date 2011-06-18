module Main where

import Core
import Logic
import Brain
import Brain.Loop

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import System.Environment
import System.IO

play :: Brain -> Brain -> Bool -> IO ()
play b1 b2 first = do
  hPutStrLn stderr $ "Player #0   [0]"
  (openingMove, brain1) <- playFirst b1
  brain2 <- playSecond b2
  go openingMove emptyBoard 1 brain2 brain1
  where
    go :: Move -> Board -> Int -> NextBrain -> NextBrain -> IO ()
    go move board ply curr next = do
      hPrint stderr move
      when (first == (ply `rem` 2 /= 0)) $ writeMove move
      let Right (rp, board') = runIdentity $ runErrorT $ runStateT (step move) board
      if ply == 200000
        then do
             hPutStrLn stderr "Game done after 100000 moves each."
             hPutStrLn stderr $ "Player #" ++ show (ply `rem` 2) ++ " has: " ++ show (score $ proponent board')
             hPutStrLn stderr $ "Player #" ++ show (1 - (ply `rem` 2)) ++ " has: " ++ show (score $ opponent board)
        else if all dead (opponent board)
             then hPutStrLn stderr $ "Player #" ++ show (ply `rem` 2) ++ " won!"
             else if all dead (proponent board)
                  then hPutStrLn stderr $ "Player #" ++ show (1 - (ply `rem` 2)) ++ " won!"
                  else do
                       hPutStrLn stderr $ "Player #" ++ show (ply `rem` 2) ++ "   [" ++ show (ply `quot` 2) ++ "]"
                       (move', brain') <- nextMove curr move board'
                       hPutStr stderr rp
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
    []                   -> play stdinBrain  stdinBrain  True
    [Left "0"]           -> play mirrorBrain stdinBrain  True
    [Left "1"]           -> play stdinBrain  mirrorBrain False
    [Right b1, Right b2] -> play b1 b2 True
    _                    -> do
      hPutStrLn stderr "Usage: ltg <brain> <brain>"
      hPutStrLn stderr $ "  where  brain `elem` " ++ show (map fst brains) 
  where
    eitherLookup e as = maybe (Left e) Right $ lookup e as

brains :: [(String, Brain)]
brains =
  [ ("nop",    nopBrain)
  , ("stdin",  stdinBrain)
  , ("mirror", mirrorBrain)
  , ("loop",   loopBrain)
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
