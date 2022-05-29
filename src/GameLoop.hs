{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GameLoop (
  loop
) where

import ConnectFour
import BoardComparison
import Control.Monad.Reader
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)
import OwnPrelude

readPlayerInput :: Player -> ReaderT Config IO Int
readPlayerInput player = do
  cfg <- liftReader config
  columnStr <- liftIO $ do
    putStr $ "Player " ++ show player ++ ", enter column number: "
    hFlush stdout
    getLine
  case (readMaybe columnStr :: Maybe Int) of
    Just column | column >= 1 && column <= cfg.columns ->
      -- convert to zero-based index
      return (column - 1)
    _ -> do
      liftIO $ putStrLn "Invalid column number, try again."
      readPlayerInput player

data PlayerKind = AI | Human deriving stock (Show, Eq)

playerKind :: Player -> PlayerKind
playerKind X = AI
playerKind O = Human
playerKind _ = error "Invalid player B"

drawCell :: CellDiff -> IO ()
drawCell (Unchanged p) = putStr $ playerDisplayString p
drawCell (Changed p) =
  -- todo show a changed cell
  putStr $ playerDisplayString p

drawDiffBoard :: Board -> Board -> ReaderT Config IO ()
drawDiffBoard prev curr = do
  cfg <- liftReader config
  let diff = diffBoards prev curr
  liftIO $ do
      forM_ diff (\row ->
        do
          forM_ row drawCell
          putStrLn "")

      forM_ [1 .. cfg.columns] (const $ putStr "-")
      putStrLn ""

      forM_ [1 .. cfg.columns] $ putStr . show
      putStrLn ""
      putStrLn ""

loopWithDiff :: Player -> Board -> Board -> ReaderT Config IO ()
loopWithDiff player prevBoard board = do
  drawDiffBoard prevBoard board
  outcome <- liftReader $ boardOutcome board
  case outcome of
    Win winner' -> liftIO $ putStrLn $ "Player " ++ (show winner') ++ " wins!"
    Draw -> liftIO $ putStrLn "Draw!"
    InProgress -> do
      case playerKind player of
        AI -> do
          board' <- liftReader $ nextMove player board
          case board' of
            Just (Definite board'') -> do
              liftIO $ putStrLn "I'll win!"
              loopWithDiff (nextPlayer player) board board''
            Just (RandomGuess board'') -> do
              liftIO $ putStrLn "I hope I'll win..."
              loopWithDiff (nextPlayer player) board board''
            Nothing -> do
              liftIO $ putStrLn "AI failed to make the next move."
        Human -> do
          column <- readPlayerInput player
          let board' = tryAddToBoard player column board
          case board' of
            Just board'' -> do
              liftIO $ putStrLn ""
              loopWithDiff (nextPlayer player) board board''
            Nothing -> do
              liftIO $ do
                putStrLn $ "Column " ++ show column ++ " is full, choose another one."
                putStrLn ""
              loopWithDiff player board board

loop :: Player -> Board -> ReaderT Config IO ()
loop player board = loopWithDiff player board board