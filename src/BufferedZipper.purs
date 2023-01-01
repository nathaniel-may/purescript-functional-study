module Data.BufferedZipper where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Array as Array
import Data.List.Lazy (List, nil, (:))
import Data.List.Lazy as List
import Data.MCache (MCache, run, uncached)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable1, unfoldr1)
import Data.ZipperM (ZipperM(..))
import Data.ZipperM as ZipperM
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import ZipperM.Utils (arrayTail', init', tail')


data BufferedZipper m a = BufferedZipper 
    (Array (m a))
    (ZipperM (MCache m) a)
    (Array (m a))

instance eqZipperM :: (Applicative m, Eq (m a), Eq a) => Eq (BufferedZipper m a) where
    eq z z' = toArray z == toArray z'

instance functorBufferedZipper :: Functor m => Functor (BufferedZipper m) where
    map f (BufferedZipper l z r) = BufferedZipper
        (map (map f) l)
        (map f z)
        (map (map f) r)

-- BufferedZipper is not a comonad because extend cannot be implemented for arbitrary effects

instance arbitraryZipperM :: (Arbitrary (m a), Arbitrary a) => Arbitrary (BufferedZipper m a) where
    arbitrary = do
        x     <- arbitrary
        xs    <- arbitrary
        bsize <- arbitrary
        pure $ mkBufferedZipper bsize x xs

mkBufferedZipper :: forall m a. Int -> a -> Array (m a) -> BufferedZipper m a
mkBufferedZipper bsize x xs =
    let { before, after } = Array.splitAt bsize xs
    in BufferedZipper [] (ZipperM.fromList1 x (List.fromFoldable $ map uncached before)) after
    
next :: forall m a. Applicative m => BufferedZipper m a -> m (Maybe (BufferedZipper m a))
next (BufferedZipper l z r) = (pure case _ of
    Just z' -> Just $ BufferedZipper l z' r
    Nothing -> case Array.uncons r of
        Nothing -> Nothing
        Just { head, tail } -> Just $ BufferedZipper
            (Array.cons head l)
            (dropLeft (snocRight z (uncached head)))
            tail
    ) <*> (run $ ZipperM.next z)

prev :: forall m a. Applicative m => BufferedZipper m a -> m (Maybe (BufferedZipper m a))
prev (BufferedZipper l z r) = (pure case _ of
    Just z' -> Just $ BufferedZipper l z' r
    Nothing -> case Array.uncons l of
        Nothing -> Nothing
        Just { head, tail } -> Just $ BufferedZipper
            tail
            (dropRight (snocLeft (uncached head) z))
            (Array.cons head r)
    ) <*> (run $ ZipperM.prev z)

-- next' :: forall m a. Applicative m => ZipperM m a -> m (ZipperM m a)
-- next' zipper = fromMaybe zipper <$> next zipper

-- prev' :: forall m a. Applicative m => ZipperM m a -> m (ZipperM m a)
-- prev' zipper = fromMaybe zipper <$> prev zipper

-- nextT :: forall m a. Applicative m => ZipperM m a -> MaybeT m (ZipperM m a)
-- nextT = MaybeT <<< next

-- prevT :: forall m a. Applicative m => ZipperM m a -> MaybeT m (ZipperM m a)
-- prevT = MaybeT <<< next

focus :: forall m a. BufferedZipper m a -> a
focus (BufferedZipper _ z _) = ZipperM.focus z

-- -- | moves the focus of the zipper to the first element
-- first :: forall m a. Monad m => ZipperM m a -> m (ZipperM m a)
-- first zipper = maybe (pure zipper) first =<< prev zipper

-- -- | moves the focus of the zipper to the last element
-- last :: forall m a. Monad m => ZipperM m a -> m (ZipperM m a)
-- last zipper = maybe (pure zipper) last =<< next zipper

toArray :: forall m a. Applicative m => BufferedZipper m a -> Array (m a)
toArray (BufferedZipper l z r) =
    (Array.reverse l)
    <> map run (ZipperM.toArray z)
    <> r

-- TODO move to an internal module
snocLeft :: forall m a. m a -> ZipperM m a -> ZipperM m a
snocLeft mx (ZipperM l z r) = ZipperM (List.snoc l mx) z r

-- TODO move to an internal module
snocRight :: forall m a. ZipperM m a -> m a -> ZipperM m a
snocRight (ZipperM l z r) mx = ZipperM l z (List.snoc r mx)

-- TODO move to an internal module
dropLeft :: forall m a. ZipperM m a -> ZipperM m a
dropLeft (ZipperM l z r) = ZipperM (fromMaybe nil $ List.init l) z r

-- TODO move to an internal module
dropRight :: forall m a. ZipperM m a -> ZipperM m a
dropRight (ZipperM l z r) = ZipperM l z (fromMaybe nil $ List.init r)
