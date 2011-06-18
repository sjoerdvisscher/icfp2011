module Cards (apply, change, changeM) where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State
import qualified Data.Vector as V

import Core

getValue :: Field -> Result Int
getValue (Value x)   = return x
getValue (Card Zero) = return 0
getValue f           = throwError $ "Field '" ++ show f ++ "' is not a value"

apply :: Field -> Field -> Result Field
apply (Value _)    _ = throwError "Value is not a function"
apply f x = do
  board <- get
  let apps = applications board + 1
  put $ board { applications = apps }
  when (apps == 1000)
    $ throwError "it's been a 1000 applications"
  case f of
    Card  I          -> f_I x
    Card  Zero       -> throwError "Zero is not a function"
    Card  Succ       -> f_succ x
    Card  Dbl        -> f_dbl x
    Card  Get        -> f_get x
    Card  Put        -> f_put x
    Card  S          -> return $ Papp1 S x
    Card  K          -> return $ Papp1 K x
    Card  Inc        -> f_inc x
    Card  Dec        -> f_dec x
    Card  Attack     -> return $ Papp1 Attack x
    Card  Help       -> return $ Papp1 Help x
    Card  Copy       -> f_copy x
    Card  Revive     -> f_revive x
    Card  Zombie     -> return $ Papp1 Zombie x
    Papp1 S f'       -> return $ Papp2 S f' x
    Papp1 K x'       -> f_K x' x
    Papp1 Attack i   -> return $ Papp2 Attack i x
    Papp1 Help i     -> return $ Papp2 Help i x
    Papp1 Zombie i   -> f_zombie i x
    Papp2 S f' g     -> f_S f' g x
    Papp2 Attack i j -> f_attack i j x
    Papp2 Help i j   -> f_help i j x
    fld              -> error $ "Can't apply to field: " ++ show fld

add :: Board -> Int -> Slot -> Slot
add b n s@(Slot f v)
  | dead s    = s
  | otherwise = Slot f $ max 0 $ min 65535 $ if zombieMode b then v - n else v + n

f_I :: Monad m => a -> m a
f_I = \x -> return x

f_succ :: Field -> StateT Board (ErrorT String Identity) Field
f_succ = \n -> do
  n' <- getValue n
  let m  = n' + 1
      m' = min m 65535
  return $ Value $ m'

f_dbl :: Field -> StateT Board (ErrorT String Identity) Field
f_dbl = \n -> do
  n' <- getValue n
  let m  = n' * 2
      m' = min m 65535
  return $ Value $ m'

f_get :: Field -> StateT Board (ErrorT String Identity) Field
f_get = \i -> do
  i' <- getSlotIndex i
  board <- get
  let slots = proponent board
  let slot = slots V.! i'
  when (dead slot) 
    $ throwError "Slot is dead"
  return $ field slot

f_put :: Monad m => t -> m Field
f_put _ = return $ Card I

f_S
  :: Field
     -> Field
     -> Field
     -> StateT Board (ErrorT String Identity) Field
f_S = \f g x -> do
  h <- f `apply` x
  y <- g `apply` x
  z <- h `apply` y
  return z

f_K :: Monad m => a -> t -> m a
f_K x _ = return x

f_inc :: Field -> StateT Board (ErrorT String Identity) Field
f_inc = \i -> do
  i' <- getSlotIndex i
  board <- get
  let slots = proponent board
  slots' <- change i' (add board 1) slots
  put $ board { proponent = slots' }
  return $ Card I

f_dec :: Field -> StateT Board (ErrorT String Identity) Field
f_dec = \i -> do
  i' <- getSlotIndex i
  board <- get
  let slots = opponent board
  slots' <- change (255 - i') (add board (-1)) slots
  put $ board { opponent = slots' }
  return $ Card I

f_attack
  :: Field
     -> Field
     -> Field
     -> StateT Board (ErrorT String Identity) Field
f_attack = \i j n -> do
  i' <- getSlotIndex i
  n' <- getValue n
  board <- get
  let subN (Slot f v)
        | v - n' < 0 = throwError "Not enough vitality for attack"
        | otherwise  = return $ Slot f (v - n')
  let proponentSlots = proponent board
  proponentSlots' <- changeM i' subN proponentSlots
  let board' = board { proponent = proponentSlots' }
  put board'
  j' <- getSlotIndex j
  let opponentSlots = opponent board'
  opponentSlots' <- change (255 - j') (add board' ((-n') * 9 `div` 10)) opponentSlots
  put $ board' { opponent = opponentSlots' }
  return $ Card I

f_help
  :: Field
     -> Field
     -> Field
     -> StateT Board (ErrorT String Identity) Field
f_help = \i j n -> do
  i' <- getSlotIndex i
  n' <- getValue n
  board <- get
  let subN (Slot f v)
        | v - n' < 0 = throwError "Not enough vitality for help"
        | otherwise  = return $ Slot f (v - n')
  let proponentSlots = proponent board
  proponentSlots' <- changeM i' subN proponentSlots
  let board' = board { proponent = proponentSlots' }
  put board'
  j' <- getSlotIndex j
  proponentSlots'' <- change j' (add board' (n' * 11 `div` 10)) proponentSlots'
  put $ board' { proponent = proponentSlots'' }
  return $ Card I

f_copy :: Field -> StateT Board (ErrorT String Identity) Field
f_copy = \i -> do
  i' <- getSlotIndex i
  board <- get
  let slots = opponent board
  return $ field $ slots V.! i'

f_revive :: Field -> StateT Board (ErrorT String Identity) Field
f_revive = \i -> do
  i' <- getSlotIndex i
  board <- get
  let slots = proponent board
      slot  = slots V.! i'
  when (dead slot) $ do
    let revive (Slot f _) = Slot f 1
    slots' <- change i' revive slots
    put $ board { proponent = slots' }
  return $ Card I

f_zombie
  :: Field -> Field -> StateT Board (ErrorT String Identity) Field
f_zombie = \i x -> do
  i' <- getSlotIndex i
  board <- get
  let slots = opponent board
      slot  = slots V.! (255 - i')
  when (alive slot)
    $ throwError "It's alive!!! not a zombie"
  let zombie _ = Slot x (-1)
  slots' <- change (255 - i') zombie slots
  put $ board { opponent = slots' }
  return $ Card I

-- Helper functions

changeM :: Monad m => Int -> (a -> m a) -> V.Vector a -> m (V.Vector a)
changeM i f vec = do
  y <- f $ vec V.! i
  return $ V.unsafeUpd vec [(i, y)]

change :: Monad m => Int -> (a -> a) -> V.Vector a -> m (V.Vector a)
change i f = changeM i (return . f)

checkSlotIndex :: Int -> Result ()
checkSlotIndex x
  | 0 <= x && x <= 255 = return ()
  | otherwise          = throwError "slot index out of bounds"

getSlotIndex :: Field -> Result Int
getSlotIndex f = getValue f >>= \i -> checkSlotIndex i >> return i

