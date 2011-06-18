module Brain.Nop where

import Logic
import Brain

-- | The brain that always suggests @Move CardToField 0 I@.
nopBrain :: Brain
nopBrain = simpleBrain (const nop)

