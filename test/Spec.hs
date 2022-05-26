module Main (main) where

import Test.Hspec
import Control.Monad.Reader
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