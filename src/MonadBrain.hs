{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadBrain (

  B,
  move,
  field, field', vitality, vitality',
  fromI,
  toBrain
  
  ) where

import Core hiding (field, vitality)
import qualified Core
import Logic
import Brain

import Data.Monoid
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Free

-- | The Brain monad.
newtype B a = B { runB :: FreeT ((,) Move) (Reader Board) a }
  deriving (Functor, Monad)

instance MonadReader Board B where
  ask = B (lift ask)
  local f (B g) = B (mapFreeT (local f) g)

-- | Execute a move and wait for the opponent to move before continuing.
move :: Move -> B ()
move m = B $ wrap (m, return ())

-- | Look up the specified proponent's field.
field :: SlotNr -> B Field
field i = asks (Core.field . (!! i) . proponent)

-- | Look up the specified proponent's field.
field' :: SlotNr -> B Field
field' i = asks (Core.field . (!! i) . opponent)

-- | Look up the specified proponent's vitality.
vitality :: SlotNr -> B Vitality
vitality i = asks (Core.vitality . (!! i) . proponent)

-- | Look up the specified opponent's vitality.
vitality' :: SlotNr -> B Vitality
vitality' i = asks (Core.vitality . (!! i) . opponent)

-- | Make sure the specified slot contains 'I' before executing the action.
fromI :: (SlotNr -> B a) -> SlotNr -> B a
fromI f i = do
  contents <- field i
  case contents of
    Card I -> return ()
    _      -> move (Move CardToField i Put)
  f i

-- | Convert a Brain monad computation to a conventional 'Brain'.
toBrain :: B a -> Brain
toBrain ~b@(B (FreeT (ReaderT f))) = Brain p1 p2
  where
    p1 = case runIdentity (f emptyBoard) of
          Left _ -> error "toBrain: no more moves"
          Right (m, b') -> return (m, toNextBrain (B b'))
    p2 = return (toNextBrain b)

toNextBrain :: B a -> NextBrain
toNextBrain (B (FreeT (ReaderT f))) =
  NextBrain $ \_ board ->
    case runIdentity (f board) of
      Left _ -> error "toNextBrain: no more moves"
      Right (m, b') -> return (m, toNextBrain (B b'))
