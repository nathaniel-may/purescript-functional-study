module Data.Zipper where

import Prelude

import Control.Extend (class Extend)
import Data.Lazy (defer, force)
import Data.List.Lazy (List, nil, zipWith)
import Data.List.Lazy as List
import Data.List.Lazy.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NonEmpty
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr1)
import Test.QuickCheck (class Arbitrary, arbitrary)


data Zipper a = Zipper (List a) a (List a)

instance eqZipper :: Eq a => Eq (Zipper a) where
    eq (Zipper l z r) (Zipper l' z' r') =
        l == l' && z == z' && r == r'

instance functorZipper :: Functor Zipper where
    map f (Zipper l z r) = Zipper (map f l) (f z) (map f r)

-- Note: Zipper is not an Applicative because it breaks the identity law

instance extendZipper :: Extend Zipper where
    extend :: forall b a. (Zipper a -> b) -> Zipper a -> Zipper b
    extend f = map f <<< duplicate
        where
        duplicate :: Zipper a -> Zipper (Zipper a)
        duplicate x = go (prev x) (next x) (singleton x) 

        go :: Maybe (Zipper a) -> Maybe (Zipper a) -> Zipper (Zipper a) -> Zipper (Zipper a)
        go Nothing Nothing zs = zs
        go xs ys zs0 =
            let zs1 = go (prev =<< xs) (next =<< ys) zs0
                zs2 = fromMaybe zs1 $ map (\xs' -> insertLeft xs' zs1) xs
                zs3 = fromMaybe zs2 $ map (\ys' -> insertRight ys' zs2) ys
            in zs3

instance arbitraryZipper :: Arbitrary a => Arbitrary (Zipper a) where
    arbitrary = do
        x <- arbitrary
        xs' <- arbitrary
        let xs = List.fromFoldable (xs' :: Array a)
        pure $ Zipper nil x xs

fromNonEmpty :: forall a. NonEmpty List a -> Zipper a
fromNonEmpty xs = Zipper nil (NonEmpty.head xs) (NonEmpty.tail xs)

fromList :: forall a. List a -> Maybe (Zipper a)
fromList xs = Zipper nil <$> List.head xs <*> List.tail xs

singleton :: forall a. a -> Zipper a
singleton x = Zipper nil x nil

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
