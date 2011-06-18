module Brain.Tom where

import Core
import Logic
import Brain
import MonadBrain
import BUtils

import Control.Monad (forever)

-- [3] 5
-- [6] inc
-- goal: [8] 6

-- heap: [100..]


tomBrain :: Brain
tomBrain = toBrain $ forever $ do
    load' 5 3
    move (Move FieldToCard 6 Succ)
    copy' 6 100 
    apply 100 3 
    copy' 100 8
