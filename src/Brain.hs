{-# LANGUAGE TupleSections #-}

module Brain (

  -- * The Brain type
  Brain(..), NextBrain(..),

  -- * Brain constructors
  simpleIOBrain, simpleBrain, brainFromMoves,

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
