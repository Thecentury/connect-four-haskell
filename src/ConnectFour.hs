{- |
Copyright: (c) 2022 Mikhail Brinchuk
SPDX-License-Identifier: MIT
Maintainer: Mikhail Brinchuk <thecentury@gmail.com>

See README for more info
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ConnectFour where

import qualified Data.List as List
import Data.Maybe (listToMaybe, catMaybes, mapMaybe)
import Control.Monad.Reader
import Data.Function ((&))
import OwnPrelude

data Config = Config {
  rows :: Int,
  columns :: Int,
  win :: Int,
  depth :: Int
} deriving Show

defaultConfig :: Config
defaultConfig = Config {
  rows = 3,
  columns = 3,
  win = 3,
  depth = 6
}

config :: Reader Config Config
config = do
  cfg <- ask
  return cfg

configOfRowsColumns :: Int -> Int -> Config
configOfRowsColumns rows columns =
  defaultConfig { rows = rows, columns = columns }

configWithWin :: Int -> Config -> Config
configWithWin win cfg =
  cfg { win = win }

data Player = O | B | X
  deriving (Show, Eq, Ord)

playerDisplayString :: Player -> String
playerDisplayString O = "o"
playerDisplayString B = "."
playerDisplayString X = "x"

playerMinimax :: Player -> [Player] -> Player
playerMinimax B _ = B
playerMinimax _ [] = B
playerMinimax O winners = List.minimum winners
playerMinimax X winners = List.maximum winners

nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer B = error "Blank cell is not a player"
nextPlayer X = O

type Row = [Player]
type Column = [Player]
type Board = [Row]

mkBoard :: Config -> Board
mkBoard cfg =
  List.replicate cfg.rows $ List.replicate cfg.columns $ B

boardRows :: Board -> [Row]
boardRows board = board

boardColumns :: Board -> Board
boardColumns board = List.transpose board

boardDiagonals :: Board -> Reader Config [Row]
boardDiagonals board = do
  cfg <- config
  let growingDiagonals = [shiftedRows shift (-1) | shift <- [0 .. cfg.columns + cfg.rows - 2]]
  let decreasingDiagonals = [shiftedRows shift 1 | shift <- [-(cfg.rows - 1) .. cfg.columns - 1]]
  let allDiagonals = List.concat [growingDiagonals, decreasingDiagonals] & List.filter (not . null) & List.concat
  return allDiagonals
  where
    shiftedRows :: Int -> Int -> [Row]
    shiftedRows shift rowMultiplier =
      board
      & mapi (\rowIndex row ->
        row
        & safeSkip (shift + rowIndex * rowMultiplier)
        & List.take 1)
      & List.filter (not . List.null)
      & List.transpose

winnerInRow :: Row -> Reader Config (Maybe Player)
winnerInRow row = do
  cfg <- config
  let toWin = cfg.win
  return $ impl toWin (B, 0) row
  where
    impl :: Int -> (Player, Int) -> Row -> Maybe Player
    impl _ _ [] = Nothing
    impl toWin _ (B : rest) = impl toWin (B, 1) rest
    impl toWin (prev, count) (player : _) | player == prev && count + 1 >= toWin = Just player
    impl toWin (prev, count) (player : rest) | player == prev = impl toWin (player, count + 1) rest
    impl toWin _ (player : rest) = impl toWin (player, 1) rest

winner :: Board -> Reader Config (Maybe Player)
winner board = do
  diagonals <- boardDiagonals board
  let toSearch = concat [boardRows board, boardColumns board, diagonals]
  winners <- sequence $ map winnerInRow $ toSearch
  return $ listToMaybe $ catMaybes winners

isFullColumn :: Column -> Bool
isFullColumn column = head column == B

isFullBoard :: Board -> Bool
isFullBoard = List.all isFullColumn . boardColumns

tryAddToColumn :: Player -> Column -> Maybe Column
tryAddToColumn player column =
  let (added, result) = List.foldr go (False, []) column
  in
    if added then Just result
    else Nothing
  where
  go :: Player -> (Bool, [Player]) -> (Bool, [Player])
  go current (True, soFar) = (True, current : soFar)
  go B (_, soFar) = (True, player : soFar)
  go current (_, soFar) = (False, current : soFar)

tryAddToBoard :: Player -> Int -> Board -> Maybe Board
tryAddToBoard player columnIndex board = fmap List.transpose $ go columnIndex [] $ boardColumns board where
  go :: Int -> [Column] -> [Column] -> Maybe [Column]
  go _ _ [] = Nothing
  go 0 soFar (column : rest) =
    let column' = tryAddToColumn player column in
    case column' of
      Nothing -> Nothing
      Just column'' -> Just $ List.reverse soFar ++ [column''] ++ rest
  go currentColumnIndex soFar (column : rest) =
    go (currentColumnIndex - 1) (column : soFar) rest

nextMoves :: Player -> Board -> [Board]
nextMoves player board =
  board
  & boardColumns
  & zipperFromList
  & zipperSelfAndRights
  & mapMaybe (\z -> tryAddToColumn player (zipperFocus z) & fmap (\column -> zipperWithFocus column z))
  & map (boardColumns . zipperToList)