module Brain.Stdin where

import Logic
import Brain

import Control.Applicative


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

