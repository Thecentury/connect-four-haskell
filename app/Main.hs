module Main (main) where

import           ConnectFour
import           Control.Monad.Reader
import           GameLoop             (loop)

cfg :: Config
cfg = Config {
  rows_ = 3,
  columns_ = 3,
  win_ = 3,
  maxDepth_ = 10
}

main :: IO ()
main = do
  putStrLn ""
  putStrLn "Welcome to the Haskell version of Connect Four game!"
  putStrLn ""

  let board = mkBoard cfg
  runReaderT (loop O board) cfg

