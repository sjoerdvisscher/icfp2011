module Brain (

  -- * The Brain type
  Brain,

  -- * Trivial brains
  nop, playerBrain

  ) where

import Core
import Logic

import Control.Applicative

-- | Suggest a move for the current proponent.
type Brain m = Board -> m Move

-- | The brain that always suggests @Move CardToField 0 I@.
nop :: Monad m => Brain m
nop _ = return (Move CardToField 0 I)

-- | The brain that reads moves from stdin.
playerBrain :: Brain IO
playerBrain _ = readMove
  where
    readMove :: IO Move
    readMove = parse <$> getLine <*> getLine <*> getLine
      where
        parse app ix card = Move (readApply app) (read ix) (read card)
        readApply "1" = CardToField
        readApply "2" = FieldToCard
