module Board where

import Cards
import Slots

type Player = [Slot]

data Board = Board { player0 :: Player, player1 :: Player }
  deriving Show

emptyBoard :: Board
emptyBoard = Board emptyPlayer emptyPlayer
  where
    emptyPlayer = replicate 255 (Slot (Function I) 10000)

