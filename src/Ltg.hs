module Ltg where

import Logic

import Control.Applicative

main = do
  putStrLn "Lambda: The Gathering version $Date:: 2011-06-17 18:39:42 +0900#$"

readMove :: IO Move
readMove = parse <$> getLine <*> getLine <*> getLine
  where
    parse app ix card = Move (readApply app) (read ix) (read card)
    readApply "1" = CardToField
    readApply "2" = FieldToCard
