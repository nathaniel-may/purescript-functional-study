module Data.Zipper where

import Prelude

import Data.List.Lazy (List, nil)
import Data.List.Lazy as List
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty)
import Data.NonEmpty as NonEmpty
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr1)


data Zipper a = Zipper (List a) a (List a)

instance eqZipper :: Eq a => Eq (Zipper a) where
    eq (Zipper l z r) (Zipper l' z' r') =
        l == l' && z == z' && r == r'

instance functorZipper :: Functor Zipper where
    map f (Zipper l z r) = Zipper (map f l) (f z) (map f r)

-- TODO add the other instances

fromNonEmpty :: forall a. NonEmpty List a -> Zipper a
fromNonEmpty xs = Zipper nil (NonEmpty.head xs) (NonEmpty.tail xs)

fromList :: forall a. List a -> Maybe (Zipper a)
fromList xs = Zipper nil <$> List.head xs <*> List.tail xs

next :: forall a. Zipper a -> Maybe (Zipper a)
next (Zipper l z r) = Zipper (List.cons z l) <$> List.head r <*> List.tail r

prev :: forall a. Zipper a -> Maybe (Zipper a)
prev (Zipper l z r) = (\z' l' -> Zipper l' z' (List.cons z r)) <$> List.head l <*> List.tail l

focus :: forall a. Zipper a -> a
focus (Zipper _ z _) = z

first :: forall a. Zipper a → Zipper a
first z = case prev z of
    Nothing -> z
    Just z' -> first z'

last :: forall a. Zipper a → Zipper a
last z = case next z of
    Nothing -> z
    Just z' -> last z'

insertRight :: forall a. a -> Zipper a -> Zipper a
insertRight x (Zipper l z r) = Zipper l z (List.cons x r)

insertLeft :: forall a. a -> Zipper a -> Zipper a
insertLeft x (Zipper l z r) = Zipper (List.cons x l) z r

toUnfoldable :: forall f. Unfoldable f => Zipper ~> f
toUnfoldable zipper = unfoldr1
    (\z -> Tuple (focus z) (next z))
    (first zipper)
