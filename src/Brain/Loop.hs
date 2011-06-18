module Brain.Loop where

import Core
import Logic
import Brain

loopBrain :: Brain
loopBrain = brainFromMoves $ cycle $ concat $ map f [0..255]
  where
    f i = [ Move FieldToCard i S
          , Move FieldToCard i Get
          , Move FieldToCard i I
          , Move FieldToCard 0 Zero
          ]

