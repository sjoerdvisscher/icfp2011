module Brain.Nop where

import Logic
import Brain

-- | The brain that always suggests 'nop'.
nopBrain :: Brain
nopBrain = simpleBrain (const nop)
