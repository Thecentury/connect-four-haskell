module OwnPrelude (
  mapi
) where

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f = go 0 where
  go _ [] = []
  go index (a:as) = f index a : go (index + 1) as