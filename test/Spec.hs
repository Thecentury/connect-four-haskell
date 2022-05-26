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
