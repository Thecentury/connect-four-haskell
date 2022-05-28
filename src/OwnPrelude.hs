module OwnPrelude (
  mapi,
  safeSkip,
  Zipper(..),
  zipperFromList,
  zipperFocus,
  zipperWithFocus,
  zipperSelfAndRights,
  zipperToList
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

data Zipper a = Zipper {
  left :: [a],
  focus :: a,
  right :: [a]
}

zipperFromList :: [a] -> Zipper a
zipperFromList list = Zipper [] (head list) (tail list)

zipperFocus :: Zipper a -> a
zipperFocus (Zipper _ f _) = f

zipperWithFocus :: a -> Zipper a -> Zipper a
zipperWithFocus f (Zipper left _ right) = Zipper left f right

zipperTryMoveRight :: Zipper a -> Maybe (Zipper a)
zipperTryMoveRight (Zipper _ _ []) = Nothing
zipperTryMoveRight (Zipper left f (r : rs)) = Just $ Zipper (f : left) r rs

zipperSelfAndRights :: Zipper a -> [Zipper a]
zipperSelfAndRights zipper = List.unfoldr gen (Just zipper)
  where
    gen :: Maybe (Zipper a) -> Maybe (Zipper a, Maybe (Zipper a))
    gen Nothing = Nothing
    gen (Just z) = Just (z, zipperTryMoveRight z)

zipperToList :: Zipper a -> [a]
zipperToList (Zipper l f r) = List.reverse l ++ [f] ++ r

