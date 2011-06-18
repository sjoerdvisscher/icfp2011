module Brain.Mirror where

import Logic
import Brain
import MonadBrain

import Data.Maybe
import Control.Monad


mirrorBrain :: Brain
mirrorBrain = toBrain $
  forever $ lastOpponentMove >>= move . fromMaybe nop
