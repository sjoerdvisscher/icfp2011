module Brain.NopReturn where

import Logic
import Brain

-- | The brain that always suggests @Move CardToField 0 I@.
-- After the users pressed enter
nopReturnBrain :: Brain
nopReturnBrain =
  simpleIOBrain (const (putStrLn "Press <Return>" >> getLine >> return nop))

