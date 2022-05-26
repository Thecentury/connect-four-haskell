module OwnPrelude (
  mapi,
  safeSkip
) where

import qualified Data.List as List

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f = go 0 where
  go _ [] = []
  go index (a:as) = f index a : go (index + 1) as

safeSkip :: Int -> [a] -> [a]
safeSkip count _ | count < 0 = []
safeSkip 0 list = list
safeSkip toSkip list = List.drop toSkip list