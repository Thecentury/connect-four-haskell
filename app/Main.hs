module Main (main) where

import ConnectFour
import GameLoop (loop)
import Control.Monad.Reader

cfg :: Config
cfg = Config {
  rows = 3,
  columns = 3,
  win = 3,
  depth = 10
}

main :: IO ()
main = do
  let board = mkBoard cfg
  runReaderT (loop O board) cfg

