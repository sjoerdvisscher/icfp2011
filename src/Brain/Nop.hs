module Brain.Nop where

import Logic
import Brain

-- | The brain that always suggests @Move CardToField 0 I@.
nopBrain :: Brain
nopBrain = simpleBrain (const nop)

brainFromMoves :: [Move] -> Brain
brainFromMoves ms = Brain
                      (return (head ms, next (tail ms)))
                      (return (next ms))
  where
    next (m:ms') = NextBrain (\_ _ -> return (m, next ms'))
    next []      = error "brainFromMoves: empty list"

