{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MonadBrain (

  B, move, toBrain
  
  ) where

import Core
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
