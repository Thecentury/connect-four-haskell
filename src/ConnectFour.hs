{- |
Copyright: (c) 2022 Mikhail Brinchuk
SPDX-License-Identifier: MIT
Maintainer: Mikhail Brinchuk <thecentury@gmail.com>

See README for more info
-}

{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module ConnectFour where

import           Control.Monad.Reader
import           Data.Function        ((&))
import qualified Data.List            as List
import           Data.Maybe           (catMaybes, listToMaybe, mapMaybe)
import           OwnPrelude
import           System.Random.Stateful

data Config = Config {
  rows_     :: Int,
  columns_  :: Int,
  win_      :: Int,
  maxDepth_ :: Int
} deriving stock (Show, Eq)

defaultConfig :: Config
defaultConfig = Config {
  rows_ = 3,
  columns_ = 3,
  win_ = 3,
  maxDepth_ = 6
}

config :: Reader Config Config
config = ask

configOfRowsColumns :: Int -> Int -> Config
configOfRowsColumns rows columns =
  defaultConfig { rows_ = rows, columns_ = columns }

configWithWin :: Int -> Config -> Config
configWithWin win cfg =
  cfg { win_ = win }

configWithDepth :: Int -> Config -> Config
configWithDepth depth cfg =
  cfg { maxDepth_ = depth }

data Player = O | B | X
  deriving stock (Show, Eq, Ord)

playerDisplayString :: Player -> String
playerDisplayString O = "o"
playerDisplayString B = "."
playerDisplayString X = "x"

playerMinimax :: Player -> [Player] -> Player
playerMinimax B _       = B
playerMinimax _ []      = B
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
  List.replicate cfg.rows_ $ List.replicate cfg.columns_ $ B

boardRows :: Board -> [Row]
boardRows board = board

boardColumns :: Board -> Board
boardColumns board = List.transpose board

boardDiagonals :: Board -> Reader Config [Row]
boardDiagonals board = do
  cfg <- config
  let growingDiagonals = [shiftedRows shift (-1) | shift <- [0 .. cfg.columns_ + cfg.rows_ - 2]]
  let decreasingDiagonals = [shiftedRows shift 1 | shift <- [-(cfg.rows_ - 1) .. cfg.columns_ - 1]]
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
  let toWin = cfg.win_
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
isFullColumn column = head column /= B

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
  go B (_, soFar)          = (True, player : soFar)
  go current (_, soFar)    = (False, current : soFar)

tryAddToBoard :: Player -> Int -> Board -> Maybe Board
tryAddToBoard player columnIndex board = fmap List.transpose $ go columnIndex [] $ boardColumns board where
  go :: Int -> [Column] -> [Column] -> Maybe [Column]
  go _ _ [] = Nothing
  go 0 soFar (column : rest) =
    let column' = tryAddToColumn player column in
    case column' of
      Nothing       -> Nothing
      Just column'' -> Just $ List.reverse soFar ++ [column''] ++ rest
  go currentColumnIndex soFar (column : rest) =
    go (currentColumnIndex - 1) (column : soFar) rest

nextMoves :: Player -> Board -> [Board]
nextMoves player board =
  board
  & boardColumns
  & zipperFromList
  & zipperSelfAndRights
  & mapMaybe (\z -> tryAddToColumn player (focus_ z) & fmap (\column -> zipperWithFocus column z))
  & map (boardColumns . zipperToList)

-------------------------------------------------------

data Winner =
  DepthExhausted
  | FoundWinner Player
  | WinnerInChildren Player
  deriving stock (Show, Eq)

winnerToPlayer :: Winner -> Player
winnerToPlayer DepthExhausted       = B
winnerToPlayer (FoundWinner p)      = p
winnerToPlayer (WinnerInChildren p) = p

data GameTreeNode = GameTreeNode {
  playerToPlay_ :: Player,
  winner_       :: Winner,
  board_        :: Board,
  depth_        :: Int
}

buildGameTree :: Player -> Board -> Reader Config (Tree GameTreeNode)
buildGameTree playerToPlay' board' = do
  cfg <- config
  result <- impl cfg.maxDepth_ 0 playerToPlay' board'
  return result
  where
    impl :: Int -> Int -> Player -> Board -> Reader Config (Tree GameTreeNode)
    impl maxDepth currentDepth playerToPlay board = do
      if currentDepth >= maxDepth then
        let treeValue' = GameTreeNode {
          playerToPlay_ = playerToPlay,
          winner_ = DepthExhausted,
          depth_ = currentDepth,
          board_ = board
        }
        in
          return $ Tree treeValue' []
      else
        do
          let nextPlayer' = nextPlayer playerToPlay
          winner' <- winner board
          (actualWinner, children) <-
            do
              case winner' of
                Just w -> return (FoundWinner w, [])
                Nothing -> do
                  children <-
                    nextMoves nextPlayer' board
                    & map (impl maxDepth (currentDepth + 1) nextPlayer')
                    & sequence
                  let childrenWinners = map (\(Tree value _) -> winnerToPlayer value.winner_) children
                  let nodeWinner = playerMinimax playerToPlay childrenWinners
                  return (WinnerInChildren nodeWinner, children)
          let v = GameTreeNode {
            playerToPlay_ = playerToPlay,
            winner_ = actualWinner,
            depth_ = currentDepth,
            board_ = board
          }
          return $ Tree v children

data AIMove =
  Definite Board
  | RandomGuess Board
  deriving stock (Show, Eq)

nextBoardFromMove :: AIMove -> Board
nextBoardFromMove (Definite b)    = b
nextBoardFromMove (RandomGuess b) = b

nextMove :: Player -> Board -> ReaderT Config IO (Maybe AIMove)
nextMove currentPlayer board = do
  tree <- liftReader $ buildGameTree (nextPlayer currentPlayer) board

  let definiteGuess = tree
                      & treeChildren
                      & filter (\child -> winnerToPlayer ((treeValue child).winner_) == currentPlayer)
                      & listToMaybe
                      & fmap (\child -> Definite (treeValue child).board_)
  let candidateOne = return definiteGuess
  let candidateTwo =
        treeChildren tree
        & filter (\child -> winnerToPlayer ((treeValue child).winner_) == B)
        & map (\child -> (treeValue child).board_)
        & randomMove
  let candidateThree =
        treeChildren tree
        & map (\child -> (treeValue child).board_)
        & randomMove

  let nextMove' = pure orElseWith <*> (pure orElseWith <*> candidateOne <*> candidateTwo) <*> candidateThree
  liftIO nextMove'

  where
    randomMove :: [Board] -> IO (Maybe AIMove)
    randomMove []      = return Nothing
    randomMove moves = do
      index <- uniformRM (0, length moves - 1) globalStdGen
      return $ Just $ RandomGuess $ moves !! index

-----------------------------------------------------------

data Outcome =
  Win Player
  | Draw
  | InProgress
  deriving stock (Show, Eq)

boardOutcome :: Board -> Reader Config Outcome
boardOutcome board = do
  winner' <- winner board
  let outcome = case winner' of
                  Just w                      -> Win w
                  Nothing | isFullBoard board -> Draw
                  Nothing                     -> InProgress
  return outcome
