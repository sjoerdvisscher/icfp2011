{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module MonadBrain (

  -- * Building brains
  B, move,

  -- * Retrieving information
  lastOpponentMove, field, field', vitality, vitality',

  -- * Converting to conventional brains
  toBrain,

  -- * Breakpoints
  break, stepwise

  ) where

import Core hiding (field, vitality)
import qualified Core
import Logic
import Brain

import Prelude hiding (break)
import Control.Monad.Reader
import Control.Monad.Free
import qualified Data.Vector as V

import System.IO

-- | The Brain monad.
newtype B a = B (FreeT ((,) Move) (ReaderT (Maybe Move, Board) IO) a)
  deriving (Functor, Monad)

instance MonadIO B where
  liftIO = B . lift . liftIO

instance MonadReader (Maybe Move, Board) B where
  ask = B (lift ask)
  local f (B g) = B (mapFreeT (local f) g)

-- | Execute a move and wait for the opponent to move before continuing.
move :: Move -> B ()
move m = B $ wrap (m, return ())

-- | Fetch the opponent's last move, if any.
lastOpponentMove :: B (Maybe Move)
lastOpponentMove = asks fst

-- | Look up the specified proponent's field.
field :: SlotNr -> B Field
field i = asks (Core.field . (V.! i) . proponent . snd)

-- | Look up the specified proponent's field.
field' :: SlotNr -> B Field
field' i = asks (Core.field . (V.! i) . opponent . snd)

-- | Look up the specified proponent's vitality.
vitality :: SlotNr -> B Vitality
vitality i = asks (Core.vitality . (V.! i) . proponent . snd)

-- | Look up the specified opponent's vitality.
vitality' :: SlotNr -> B Vitality
vitality' i = asks (Core.vitality . (V.! i) . opponent . snd)

-- | Convert a Brain monad computation to a conventional 'Brain'.
toBrain :: B a -> Brain
toBrain ~b@(B (FreeT (ReaderT f))) = Brain p1 p2
  where
    p1 = do
      ei <- f (Nothing, emptyBoard)
      case ei of
        Left _        -> error "toBrain: no more moves"
        Right (m, b') -> return (m, toNextBrain (B b'))
    p2 = return (toNextBrain b)

toNextBrain :: B a -> NextBrain
toNextBrain (B (FreeT (ReaderT f))) =
  NextBrain $ \opponentMove board -> do
    ei <- f (Just opponentMove, board)
    case ei of
      Left _        -> error "toNextBrain: no more moves"
      Right (m, b') -> return (m, toNextBrain (B b'))


-- Breakpoints

-- | Insert a breakpoint.
break :: MonadIO m => m ()
break = liftIO $ do
  hPutStrLn stderr "Breakpoint hit. Press <Enter> to continue."
  void getLine

-- | Insert a breakpoints around every move in the given brain.
stepwise :: B a -> B a
stepwise (B b) = B (mapFreeT (break >>) b)
