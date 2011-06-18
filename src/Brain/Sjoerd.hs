module Brain.Sjoerd where

import Core
import Logic
import Brain

sjoerdBrain :: Brain
sjoerdBrain = brainFromMoves . ($ repeat nop) 
  $ attackAll 255 255

attackAll :: Int -> Int -> [Move] -> [Move]
attackAll 1 _ = id
attackAll me him = 
    attack me him 5556 1
  . attack (me - 1) him 5556 1
  . attackAll (me - 2) (him - 1)
  
numToMoves :: Int -> Int -> [Move] -> [Move]
numToMoves 0 slot ms = Move FieldToCard slot Zero : ms
numToMoves n slot ms
  | odd n     = numToMoves (n - 1)     slot (Move CardToField slot Succ : ms)
  | otherwise = numToMoves (n `div` 2) slot (Move CardToField slot Dbl  : ms)

numToPrefix :: Int -> Int -> [Move] -> [Move]
numToPrefix 0 _ = id
numToPrefix n slot
  | odd n     = prefix Succ slot . numToPrefix (n - 1)     slot
  | otherwise = prefix Dbl  slot . numToPrefix (n `div` 2) slot

fieldToField :: Int -> Int -> [Move] -> [Move]
fieldToField i j = 
    prefix Get i
  . numToPrefix j i
  . (Move FieldToCard i Zero :)

fieldToNum :: Int -> Int -> [Move] -> [Move]
fieldToNum i j = 
    numToPrefix j i
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
  . fieldToNum slot i
  . fieldToNum slot j
  . fieldToNum slot n
  