module Brain (

  -- * The Brain type
  Brain,

  -- * Trivial brains
  nop

  ) where

import Core
import Logic

-- | Suggest a move for the current proponent.
type Brain m = Board -> m Move

-- | The brain that always suggests @Move CardToField 0 I@.
nop :: Monad m => Brain m
nop _ = return (Move CardToField 0 I)
