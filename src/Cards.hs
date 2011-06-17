module Cards where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

import Core

type Card = Field

card :: String -> Card
card s = case lookup s cards of
           Just c -> c
           Nothing -> error "Cards.card: doesn't exist"

cards :: [(String, Card)]
cards =
 [ ("I",       Function (\x -> return x))
 , ("zero",    Value 0)
 , ("succ",    Function (\n -> do
                               n' <- getValue n
                               let m  = n' + 1
                                   m' = min m 65535
                               return $ Value $ m'
                        ))
 , ("dbl",     Function (\n -> do
                               n' <- getValue n
                               let m  = n' * 2
                                   m' = min m 65535
                               return $ Value $ m'
                        ))
 , ("get",     Function (\i -> do
                               i' <- getSlotIndex i
                               board <- get
                               let slots = proponent board
                               let slot = slots !! i'
                               when (dead slot) 
                                 $ throwError "Slot is dead"
                               return $ field slot
                        ))
 , ("put",     Function (\_ -> return $ card "I"))
 , ("S",       Function (\f -> return $ Function
                        (\g -> return $ Function
                        (\x -> do
                               h <- f `apply` x
                               y <- g `apply` x
                               z <- h `apply` y
                               return z
                        ))))
  , ("K",      Function (\x -> return $ Function (\_ -> return x)))
  , ("inc",    Function (\i -> do
                               i' <- getSlotIndex i
                               board <- get
                               let inc (Slot f v)
                                     | v > 0 && v < 65535 = Slot f (v + 1)
                                     | otherwise          = Slot f v
                               let slots = proponent board
                               slots' <- change i' inc slots
                               put $ board { proponent = slots' }
                               return $ card "I"
                        ))
  , ("dec",    Function (\i -> do
                               i' <- getSlotIndex i
                               board <- get
                               let dec (Slot f v)
                                     | v > 0     = Slot f (v - 1)
                                     | otherwise = Slot f v
                               let slots = opponent board
                               slots' <- change (255 - i') dec slots
                               put $ board { opponent = slots' }
                               return $ card "I"
                        ))
  , ("attack", Function (\i -> return $ Function
                        (\j -> return $ Function
                        (\n -> do
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
                               let subN' s@(Slot f v)
                                     | dead s    = s
                                     | otherwise = Slot f $ max 0 (v - n' * 9 `div` 10)
                               let opponentSlots = opponent board'
                               opponentSlots' <- change (255 - j') subN' opponentSlots
                               put $ board' { opponent = opponentSlots' }
                               return $ card "I"
                         ))))
  , ("help",   Function (\i -> return $ Function
                        (\j -> return $ Function
                        (\n -> do
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
                               let addN' s@(Slot f v)
                                     | dead s    = s
                                     | otherwise = Slot f $ min 65535 (v + n' * 11 `div` 10)
                               proponentSlots'' <- change j' addN' proponentSlots'
                               put $ board' { proponent = proponentSlots'' }
                               return $ card "I"
                         ))))
  , ("copy",    Function (\i -> do
                                i' <- getSlotIndex i
                                board <- get
                                let slots = opponent board
                                return $ field $ slots !! i'
                ))
  , ("revive",  Function (\i -> do
                                i' <- getSlotIndex i
                                board <- get
                                let slots = proponent board
                                    slot  = slots !! i'
                                when (dead slot) $ do
                                  let revive (Slot f _) = Slot f 1
                                  slots' <- change i' revive slots
                                  put $ board { proponent = slots' }
                                return $ card "I"
                ))
  , ("zombie",  Function (\i -> return $ Function
                         (\x -> do
                                i' <- getSlotIndex i
                                board <- get
                                let slots = opponent board
                                    slot  = slots !! i'
                                when (alive slot)
                                  $ throwError "It's alive!!! not a zombie"
                                let zombie _ = Slot x (-1)
                                slots' <- change (255 - i') zombie slots
                                put $ board { opponent = slots' }
                                return $ card "I"
                )))
  ]

-- Helper functions

changeM :: Monad m => Int -> (a -> m a) -> [a] -> m [a]
changeM i f xs = do
  y <- f $ xs !! i
  return $ take i xs ++ y : drop (i + 1) xs

change :: Monad m => Int -> (a -> a) -> [a] -> m [a]
change i f = changeM i (return . f)

checkSlotIndex :: Int -> Result ()
checkSlotIndex x
  | 0 <= x && x <= 255 = return ()
  | otherwise          = throwError "slot index out of bounds"

getSlotIndex :: Field -> Result Int
getSlotIndex f = getValue f >>= \i -> checkSlotIndex i >> return i
