module Brain.Tom where

import Core
import Logic
import Brain
import MonadBrain
import BUtils

import Control.Monad (forever)
import Data.Foldable
import Prelude hiding (break)

tomBrain :: Brain
tomBrain = toBrain $ do
  -- todo: pick random start slot
  -- Attack first
  attack 255 0 5556 123
  attack 254 0 5556 123

  -- break
  -- todo: get their live slots, sorted by expression size / importantness
  for_ [0,2..255] $ \i -> do
    -- todo: get my live/empty slot, sorted by vitality
    Put `applyCardToField` 253
    253 `applyFieldToCard` Help
    253 `applyInt` i
    253 `applyInt` (i + 1)
    K `applyCardToField` 253
    S `applyCardToField` 253

    -- todo: get my live/empty slot, sorted by vitality
    load 10000 252
    K `applyCardToField` 252
    253 `apply` 252

    -- alert "before zombie"
    -- break

    -- todo: get their dead slots
    251 `applyFieldToCard` Zombie
    251 `applyInt` if i == 0 then 0 else (255 - (i - 2))
    251 `apply` 253

    alert "after zombie"
    -- break

  -- todo: keep cycling ^ until opponent is dead, remove nops
  forever (move nop)

interessting :: B [SlotNr]
interessting = do
  return $ [0..]

