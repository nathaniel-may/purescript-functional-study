module Data.ZipperM where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Array as Array
import Data.List.Lazy (List, nil, (:))
import Data.List.Lazy as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable1, unfoldr1)
import Test.QuickCheck (class Arbitrary, arbitrary)
import ZipperM.Utils (init', tail')


-- TODO makek a new type with buffers on either side that can drop from the back. Maybe use Data.Sequence?
data ZipperM m a = ZipperM (List (m a)) a (List (m a))

-- TODO implement without toArray. Maybe dump the Applicative constraint?
instance eqZipperM :: (Applicative m, Eq (m a), Eq a) => Eq (ZipperM m a) where
    eq z z' = toArray z == toArray z'

-- really only practical for non effectful zippers. TODO should it be included?
instance unfoldable1ZipperM :: Applicative m => Unfoldable1 (ZipperM m) where
    unfoldr1 :: forall a b. (b -> Tuple a (Maybe b)) -> b -> ZipperM m a
    unfoldr1 f z = case f z of
        Tuple x Nothing -> ZipperM nil x nil
        Tuple x (Just y) -> ZipperM nil x (pure <$> unfoldr1 f y)

instance functorZipperM :: Functor m => Functor (ZipperM m) where
    map f (ZipperM l x r) = ZipperM (map (map f) l) (f x) (map (map f) r)

-- ZipperM is not a comonad because extend cannot be implemented for arbitrary effects

instance arbitraryZipperM :: (Arbitrary (m a), Arbitrary a) => Arbitrary (ZipperM m a) where
    arbitrary = do
        x <- arbitrary
        xs <- arbitrary
        pure $ fromArray1 x xs

-- TODO rename "fromList1"
mkZipperM :: forall m a. a -> List (m a) -> ZipperM m a
mkZipperM = ZipperM nil

-- TODO rename "fromList"
mkZipperM' :: forall m a. Applicative m => List (m a) -> m (Maybe (ZipperM m a))
mkZipperM' xs = map (\z -> ZipperM nil z xs) <$> (sequence $ List.head xs)

fromArray1 :: forall m a. a -> Array (m a) -> ZipperM m a
fromArray1 x xs = ZipperM nil x (Array.toUnfoldable xs)

-- TODO add fromArray?

next :: forall m a. Applicative m => ZipperM m a -> m (Maybe (ZipperM m a))
next (ZipperM left z right) =
    map (\z' -> ZipperM (left `List.snoc` pure z) z' (tail' right)) <$> (sequence $ List.head right)

prev :: forall m a. Applicative m => ZipperM m a -> m (Maybe (ZipperM m a))
prev (ZipperM left z right) =
    map (\z' -> ZipperM (init' left) z' (pure z `List.cons` right)) <$> (sequence $ List.last left)

next' :: forall m a. Applicative m => ZipperM m a -> m (ZipperM m a)
next' zipper = fromMaybe zipper <$> next zipper

prev' :: forall m a. Applicative m => ZipperM m a -> m (ZipperM m a)
prev' zipper = fromMaybe zipper <$> prev zipper

nextT :: forall m a. Applicative m => ZipperM m a -> MaybeT m (ZipperM m a)
nextT = MaybeT <<< next

prevT :: forall m a. Applicative m => ZipperM m a -> MaybeT m (ZipperM m a)
prevT = MaybeT <<< next

focus :: forall m a. ZipperM m a -> a
focus (ZipperM _ z _) = z

-- | moves the focus of the zipper to the first element
first :: forall m a. Monad m => ZipperM m a -> m (ZipperM m a)
first zipper = maybe (pure zipper) first =<< prev zipper

-- | moves the focus of the zipper to the last element
last :: forall m a. Monad m => ZipperM m a -> m (ZipperM m a)
last zipper = maybe (pure zipper) last =<< next zipper

toList :: forall m a. Applicative m => ZipperM m a -> List (m a)
toList (ZipperM l z r) = List.reverse l <> (pure z : nil) <> r

toArray :: forall m a. Applicative m => ZipperM m a -> Array (m a)
toArray (ZipperM l z r) = List.toUnfoldable (List.reverse l) <> [pure z] <> List.toUnfoldable r
