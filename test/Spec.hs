module Main (main) where

import Test.Hspec
import Control.Monad.Reader
import Data.Function ((&))
import ConnectFour

main :: IO ()
main = hspec $ do
--  describe "Prelude.head" $ do
--    it "returns the first element of a list" $ do
--      head [23 ..] `shouldBe` (23 :: Int)
--
--    it "returns the first element of an *arbitrary* list" $
--      property $ \x xs -> head (x:xs) == (x :: Int)
--
--    it "throws an exception if used with an empty list" $ do
--      evaluate (head []) `shouldThrow` anyException

  describe "Diagonals" $ do
    it "calculates diagonals of a 1x1 board" $ do
      let board = [[X]]
      let cfg = configOfRowsColumns 1 1
      let diagonals = runReader (boardDiagonals board) cfg
      diagonals `shouldBe` [[X], [X]]

    it "calculates diagonals of a 2x1 board" $ do
      let board = [[X], [O]]
      let cfg = configOfRowsColumns 2 1
      let diagonals = runReader (boardDiagonals board) cfg
      diagonals `shouldBe` [[X], [O], [O], [X]]

    it "calculates diagonals of a 1x2 board" $ do
      let board = [[X, O]]
      let cfg = configOfRowsColumns 1 2
      let diagonals = runReader (boardDiagonals board) cfg
      diagonals `shouldBe` [[X], [O], [X], [O]]

    it "calculates diagonals of a 1x3 board" $ do
      let board = [[X, O, X]]
      let cfg = configOfRowsColumns 1 3
      let diagonals = runReader (boardDiagonals board) cfg
      diagonals `shouldBe` [[X], [O], [X], [X], [O], [X]]

    it "calculates diagonals of a 3x1 board" $ do
      let board = [[X], [O], [X]]
      let cfg = configOfRowsColumns 3 1
      let diagonals = runReader (boardDiagonals board) cfg
      diagonals `shouldBe` [[X], [O], [X], [X], [O], [X]]

    it "calculates diagonals of a 2x2 board" $ do
      let board =
            [
              [X, B],
              [O, B]
            ]
      let cfg = configOfRowsColumns 2 2
      let diagonals = runReader (boardDiagonals board) cfg
      let expectedDiagonals =
            [
              [X],
              [B, O],
              [B],
              [O],
              [X, B],
              [B]
            ]
      diagonals `shouldBe` expectedDiagonals

  describe "Winner" $ do
    let cfg = configOfRowsColumns 2 2 & configWithWin 2

    it "no winner" $ do
      let board =
            [
              [X, B],
              [O, B]
            ]
      let winner_ = runReader (winner board) cfg
      winner_ `shouldBe` Nothing

    it "winner in a row" $ do
      let board =
            [
              [X, X],
              [O, B]
            ]
      let winner_ = runReader (winner board) cfg
      winner_ `shouldBe` Just X

    it "winner in a column" $ do
      let board =
            [
              [X, B],
              [X, O]
            ]
      let winner_ = runReader (winner board) cfg
      winner_ `shouldBe` Just X

    it "winner in a diagonal" $ do
      let board =
            [
              [X, B],
              [O, X]
            ]
      let winner_ = runReader (winner board) cfg
      winner_ `shouldBe` Just X

  describe "TryAdd" $ do
    it "to a full column" $ do
      let column' = tryAddToColumn O [X, O]
      column' `shouldBe` Nothing

    it "to a not empty column" $ do
      let column' = tryAddToColumn X [B, O]
      column' `shouldBe` Just [X, O]

  describe "NextMoves" $ do
    it "of an empty 1x1 board" $ do
      let board = [[B]]
      let nextMoves' = nextMoves O board
      nextMoves' `shouldBe` [[[O]]]

    it "of an empty 2x1 board" $ do
      let board = [[B, B]]
      let nextMoves' = nextMoves O board
      let expectedMoves =
            [
              [[O, B]],
              [[B, O]]
            ]
      nextMoves' `shouldBe` expectedMoves

    it "of an empty 2x2 board" $ do
      let board = [[B, B], [B, B]]
      let nextMoves' = nextMoves O board
      let expectedMoves =
            [
              [
                [B, B],
                [O, B]
              ],
              [
                [B, B],
                [B, O]
              ]
            ]
      nextMoves' `shouldBe` expectedMoves

    it "of a non-empty 2x2 board" $ do
      let board = [[B, B], [X, B]]
      let nextMoves' = nextMoves O board
      let expectedMoves =
            [
              [
                [O, B],
                [X, B]
              ],
              [
                [B, B],
                [X, O]
              ]
            ]
      nextMoves' `shouldBe` expectedMoves

    it "of an 2x2 board with one full column" $ do
      let board = [[O, B], [X, B]]
      let nextMoves' = nextMoves O board
      let expectedMoves =
            [
              [
                [O, B],
                [X, O]
              ]
            ]
      nextMoves' `shouldBe` expectedMoves

    it "of a full 2x2 board" $ do
      let board = [[O, X], [X, O]]
      let nextMoves' = nextMoves O board
      nextMoves' `shouldBe` []