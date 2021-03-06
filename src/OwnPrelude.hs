module OwnPrelude (
  mapi,
  safeSkip,
  Zipper(..),
  zipperFromList,
  zipperWithFocus,
  zipperSelfAndRights,
  zipperToList,
  Tree(..),
  treeValue,
  treeChildren,
  orElseWith,
  orElseWithIO,
  liftReader
) where

import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.List              as List

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f = go 0 where
  go _ []         = []
  go index (a:as) = f index a : go (index + 1) as

safeSkip :: Int -> [a] -> [a]
safeSkip count _ | count < 0 = []
safeSkip 0 list = list
safeSkip toSkip list = List.drop toSkip list

data Zipper a = Zipper {
  left_  :: [a],
  focus_ :: a,
  right_ :: [a]
}

zipperFromList :: [a] -> Zipper a
zipperFromList list = Zipper [] (head list) (tail list)

zipperWithFocus :: a -> Zipper a -> Zipper a
zipperWithFocus f (Zipper left _ right) = Zipper left f right

zipperTryMoveRight :: Zipper a -> Maybe (Zipper a)
zipperTryMoveRight (Zipper _ _ [])          = Nothing
zipperTryMoveRight (Zipper left f (r : rs)) = Just $ Zipper (f : left) r rs

zipperSelfAndRights :: Zipper a -> [Zipper a]
zipperSelfAndRights zipper = List.unfoldr gen (Just zipper)
  where
    gen :: Maybe (Zipper a) -> Maybe (Zipper a, Maybe (Zipper a))
    gen Nothing  = Nothing
    gen (Just z) = Just (z, zipperTryMoveRight z)

zipperToList :: Zipper a -> [a]
zipperToList (Zipper l f r) = List.reverse l ++ [f] ++ r

-----------------------------------------------------------

data Tree a = Tree a [Tree a]

treeValue :: Tree a -> a
treeValue (Tree v _) = v

treeChildren :: Tree a -> [Tree a]
treeChildren (Tree _ c) = c

-----------------------------------------------------------

orElseWith :: Maybe a -> Maybe a -> Maybe a
orElseWith _ (Just a)    = Just a
orElseWith other Nothing = other

orElseWithIO :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
orElseWithIO other first = do
  first' <- first
  case first' of
    Just _ -> return first'
    Nothing -> other
-----------------------------------------------------------

liftReader :: Monad m => ReaderT r Identity b -> ReaderT r m b
liftReader = mapReaderT (pure . runIdentity)
