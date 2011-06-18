module Brain.Tom where

import Core
import Logic
import Brain
import MonadBrain
import BUtils

import Control.Monad (forever)
import Control.Monad.IO.Class
import Data.Foldable
import Prelude hiding (break)
import System.IO

tomBrain :: Brain
tomBrain = toBrain $ do
  -- for_ [255,253..1] $ \i -> do
  --   attack i       (i `div` 2 + 128) 5556 123
  --   attack (i + 1) (i `div` 2 + 128) 5556 123

  -- Attack first
  attack 255 0 5556 123
  attack 254 0 5556 123

  -- break
  for_ [0,2..255] $ \i -> do
    Put `applyCardToField` 253
    253 `applyFieldToCard` Help
    253 `applyInt` i
    253 `applyInt` (i + 1)
    K `applyCardToField` 253
    S `applyCardToField` 253
    load 10000 252
    K `applyCardToField` 252
    253 `apply` 252

    -- alert "before zombie"
    -- break

    251 `applyFieldToCard` Zombie
    251 `applyInt` 0
    251 `apply` 253

    -- alert "after zombie"
    -- break

  -- for_ [255,252..0] $ \i -> do
  --   attack i       (128 - (i `div` 3)) 3705 123
  --   attack (i + 1) (128 - (i `div` 3)) 3705 123
  --   attack (i + 2) (128 - (i `div` 3)) 3705 123

  forever (move nop)

alert :: MonadIO m => String -> m ()
alert s = liftIO (hPutStrLn stderr s)

