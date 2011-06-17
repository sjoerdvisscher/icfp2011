module Ltg where

import Core
import Cards
import Logic

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Data.Maybe

main = do
  putStrLn "Lambda: The Gathering, by Simons Fanboys"
  go emptyBoard 0
  where
    go s i = do
      putStrLn $ "Player #" ++ show i
      m <- readMove
      case runIdentity $ runErrorT $ runStateT (step m) s of
        Right (rp, s') -> putStrLn rp >> go s' (1 - i)
        Left err       -> putStrLn err
    step m = do
      mb <- turn m
      s <- report
      return $ maybe "" (++"\n") mb ++ s

report :: Result String
report = do
  board <- get
  let slots = opponent board
  let pr (Slot (Card I) 10000, _) = ""
      pr tuple                    = show tuple ++ "\n"
  return $ concatMap pr (zip slots [0..])

readMove :: IO Move
readMove = parse <$> getLine <*> getLine <*> getLine
  where
    parse app ix card = Move (readApply app) (read ix) (read card)
    readApply "1" = CardToField
    readApply "2" = FieldToCard

