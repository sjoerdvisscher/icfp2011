{-# LANGUAGE TupleSections #-}

module Brain (

  -- * The Brain type
  Brain(..), NextBrain(..),

  -- * Brain constructors
  simpleIOBrain, simpleBrain, brainFromMoves,

  -- * Trivial brains
  nopBrain, stdinBrain, mirrorBrain

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
        parse "1" l1 l2 = Move CardToField (read l2) (read l1)
        parse "2" l1 l2 = Move FieldToCard (read l1) (read l2)
        parse _   _  _  = error "parse: Not valid ApplyMode"

mirrorBrain :: Brain
mirrorBrain = Brain
  { playFirst  = return (nop, nextBrain) 
  , playSecond = return nextBrain
  }
  where
    nextBrain = NextBrain (\m _ -> return (m, nextBrain))

brainFromMoves :: [Move] -> Brain
brainFromMoves ms = Brain
                      (return (head ms, next (tail ms)))
                      (return (next ms))
  where
    next (m:ms') = NextBrain (\_ _ -> return (m, next ms'))
    next []      = error "brainFromMoves: empty list"