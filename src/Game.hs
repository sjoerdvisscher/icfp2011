module Game where

import Data.Maybe
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.State

data Board = Board { proponent :: Player, opponent :: Player }
  deriving Show

emptyBoard :: Board
emptyBoard = Board emptyPlayer emptyPlayer
  where
    emptyPlayer = replicate 255 (Slot (card "I") 10000)

type Player = [Slot]

data Slot = Slot { field :: Field, vitality :: Vitality }
  deriving Show

type Vitality = Int

type Result = StateT Board (ErrorT String Identity)

data Field
  = Value Int
  | Function (Field -> Result Field)

instance Show Field where
  show (Value x)    = "Value " ++ show x
  show (Function _) = "Function _"

getValue :: Field -> Result Int
getValue (Value x) = return $ x
getValue _         = throwError "Field is not a value"

apply :: Field -> Field -> Result Field
apply (Function f) x = f x
apply _            _ = throwError "Field is not a function"

slotNr :: Slot -> Maybe Int
slotNr (Slot (Value x) _)
  | 0 <= x && x <= 255    = Just x
slotNr _                  = Nothing

dead :: Slot -> Bool
dead (Slot _ x)
  | x == -1 || x == 0 = True
dead _                = False

alive :: Slot -> Bool
alive = not . dead

-- Functions

change :: Int -> (a -> a) -> [a] -> [a]
change i f xs = take i xs ++ (f $ xs !! i) : drop (i + 1) xs

card :: String -> Field
card = fromJust . (`lookup` cards)

cards :: [(String, Field)]
cards =
 [ ("I",    Function (\x -> return x))
 , ("zero", Value 0)
 , ("succ", Function (\n -> do
                            n' <- getValue n
                            return $ Value $ n' + 1
                     ))
 , ("dbl",  Function (\n -> do
                            n' <- getValue n
                            return $ Value $ n' * 2
                     ))
 , ("get",  Function (\i -> do
                            board <- get
                            let slots = proponent board
                            i' <- getValue i
                            return $ field $ slots !! i'
                     ))
 , ("put",  Function (\_ -> return $ card "I"))
 , ("S",    Function (\f -> return $ Function
                     (\g -> return $ Function
                     (\x -> do
                            h <- f `apply` x
                            y <- g `apply` x
                            z <- h `apply` y
                            return z
                     ))))
  , ("K",   Function (\x -> return $ Function (\_ -> return x)))
  , ("inc", Function (\i -> do
                            i' <- getValue i
                            board <- get
                            let inc (Slot f v) = Slot f (v + 1)
                            let slots = proponent board
                            let slots' = change i' inc slots
                            put $ board { proponent = slots' }
                            return $ card "I"
                     ))
  ]

