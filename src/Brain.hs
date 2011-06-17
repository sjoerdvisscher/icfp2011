module Brain (Brain) where

import Core
import Logic

-- | Suggest a move for the current proponent.
type Brain m = Board -> m Move
