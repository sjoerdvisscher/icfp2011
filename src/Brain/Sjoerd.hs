module Brain.Sjoerd where

import Core
import Logic
import Brain

sjoerdBrain :: Brain
sjoerdBrain = brainFromMoves . ($ []) 
  $ numToMoves 255 1
  . numToMoves 254 2
  . numToMoves 10000 3
  . attack 1 1 3 4
  . attack 2 1 3 4
  
numToMoves :: Int -> Int -> [Move] -> [Move]
numToMoves 0 slot ms = Move FieldToCard slot Zero : ms
numToMoves n slot ms
  | odd n     = Move CardToField slot Succ : numToMoves (n - 1)     slot ms
  | otherwise = Move CardToField slot Dbl  : numToMoves (n `div` 2) slot ms

numToPrefix :: Int -> Int -> [Move] -> [Move]
numToPrefix 0 slot = id
numToPrefix n slot
  | odd n     = prefix Succ slot . numToPrefix (n - 1)     slot
  | otherwise = prefix Dbl  slot . numToPrefix (n `div` 2) slot

fieldToField :: Int -> Int -> [Move] -> [Move]
fieldToField i j = 
    prefix Get i
  . numToPrefix j i
  . (Move FieldToCard i Zero :)

prefix :: Card -> Int -> [Move] -> [Move]
prefix c slot ms =
    Move CardToField slot K
  : Move CardToField slot S
  : Move FieldToCard slot c
  : ms

attack :: Int -> Int -> Int -> Int -> [Move] -> [Move]
attack i j n slot =
    (Move FieldToCard slot Attack :)
  . fieldToField slot i
  . fieldToField slot j
  . fieldToField slot n
  