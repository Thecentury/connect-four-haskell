{-# LANGUAGE DerivingStrategies #-}

module BoardComparison where

import ConnectFour

data CellDiff =
  Unchanged Player
  | Changed Player
  deriving stock (Show, Eq)

cellDiff :: Player -> Player -> CellDiff
cellDiff prev curr | prev == curr = Unchanged prev
cellDiff _ curr                   = Changed curr

diffBoards :: Board -> Board -> [[CellDiff]]
diffBoards = zipWith (zipWith cellDiff)