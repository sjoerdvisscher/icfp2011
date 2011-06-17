{-# LANGUAGE TupleSections #-}

module Brain (

  -- * The Brain type
  Brain(..), NextBrain(..),

  -- * Brain constructors
  simpleIOBrain, simpleBrain, 

  -- * Trivial brains
  nopBrain, stdinBrain

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

-- | The brain that always suggests @Move CardToField 0 I@.
nopBrain :: Brain
nopBrain = simpleBrain (const nop)

nop :: Move
nop = Move CardToField 0 I

-- | The brain that reads moves from stdin.
stdinBrain :: Brain
stdinBrain = simpleIOBrain (const readMove)
  where
    readMove :: IO Move
    readMove = parse <$> getLine <*> getLine <*> getLine
      where
        parse app ix card = Move (readApply app) (read ix) (read card)
        readApply "1" = CardToField
        readApply "2" = FieldToCard
        readApply _   = error "readApply: Not valid ApplyMode"

