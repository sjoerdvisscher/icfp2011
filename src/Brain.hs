{-# LANGUAGE TupleSections #-}

module Brain (

  -- * The Brain type
  Brain(..), NextBrain(..),

  -- * Brain constructors
  simpleIOBrain, simpleBrain, brainFromMoves,

  SlotNr,

  -- * Cool moves
  load', copy', apply, applyInt, composeCard

  ) where

import Core
import Logic

import Control.Applicative

data Brain = Brain
  { playFirst  :: IO (Move, NextBrain)
  , playSecond :: IO NextBrain
  }

data NextBrain = NextBrain
  { nextMove :: Move -> Board -> IO (Move, NextBrain)
  }

simpleIOBrain :: (Board -> IO Move) -> Brain
simpleIOBrain f = Brain ((, nextBrain) <$> f emptyBoard) (return nextBrain)
  where
    nextBrain = NextBrain (\_ board -> (, nextBrain) <$> f board)

simpleBrain :: (Board -> Move) -> Brain
simpleBrain f = simpleIOBrain (return . f)

brainFromMoves :: [Move] -> Brain
brainFromMoves ms = Brain
                      (return (head ms, next (tail ms)))
                      (return (next ms))
  where
    next (m:ms') = NextBrain (\_ _ -> return (m, next ms'))
    next []      = error "brainFromMoves: empty list"


-- | Index of slot between 0 and 255
type SlotNr = Int
 
-- | Loads Int into a Slot
-- 
-- where the slot currently is I 
load' :: Int -> SlotNr -> [Move] -> [Move]
load' 0 slot ms = Move FieldToCard slot Zero : ms
load' n slot ms
  | odd n     = load' (n - 1)     slot (Move CardToField slot Succ : ms)
  | otherwise = load' (n `div` 2) slot (Move CardToField slot Dbl  : ms)

-- | Copies field of src to field of dest
-- 
-- where dest currently is I 
copy' :: SlotNr -> SlotNr -> [Move] -> [Move]
copy' src dest =
    load' src dest
  . (Move CardToField dest Get :)

-- | Applies function in slot `func` to value in slot `arg`, replacing slot with
-- value
apply :: SlotNr -> SlotNr -> [Move] -> [Move]
apply func arg = 
    composeCard func Get
  . applyInt func arg
  . (Move FieldToCard func Zero :)

-- | Applies function in slot to value x, replacing slot with value
applyInt :: SlotNr -> Int -> [Move] -> [Move]
applyInt slot 0 = (Move FieldToCard slot Zero :)
applyInt slot n
  | odd n     = composeCard slot Succ . applyInt slot (n - 1)    
  | otherwise = composeCard slot Dbl  . applyInt slot (n `div` 2)

-- | Composes function in slot with card, replacing field slot with the
-- composition
composeCard :: SlotNr -> Card -> [Move] -> [Move]
composeCard slot card =
    (Move CardToField slot K :)
  . (Move CardToField slot S :)
  . (Move FieldToCard slot card :)

