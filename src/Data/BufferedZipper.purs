module Data.BufferedZipper where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.List.Lazy (nil)
import Data.List.Lazy as List
import Data.MCache (MCache, run, uncached)
import Data.MCache as MCache
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.ZipperM (ZipperM(..))
import Data.ZipperM as ZipperM
import Test.QuickCheck (class Arbitrary, arbitrary)
import Utils (init', tail')


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

-- TODO double check this?
-- BufferedZipper is not a comonad because extend cannot be implemented for arbitrary effects

instance arbitraryZipperM :: (Arbitrary (m a), Arbitrary a) => Arbitrary (BufferedZipper m a) where
    arbitrary = do
        buffer <- arbitrary
        xs <- arbitrary
        pure $ BufferedZipper [] buffer xs

-- | buffersize must be at least 1.
-- | For no buffer, use `Zipper` and for an infinite buffer use `ZipperM`
-- TODO use NonEmpty Array (m a)?
mkBufferedZipper :: forall m a. Applicative m => Int -> Array (m a) -> m (Maybe (BufferedZipper m a))
mkBufferedZipper bsize xs
    | bsize < 1 = pure Nothing
    | otherwise = sequence $ (\{ head, tail } ->
        let { before, after } = Array.splitAt bsize tail
        in map (\h -> BufferedZipper [] (ZipperM.fromList1 h (List.fromFoldable $ map uncached before)) after) head
    ) <$> (Array.uncons xs)
    
-- TODO drop constraint to Applicative
next :: forall m a. Monad m => BufferedZipper m a -> m (Maybe (BufferedZipper m a))
next (BufferedZipper l z r) = do
    mz' <- (run $ ZipperM.next z)
    case mz' of
        Just z' -> pure <<< Just $ BufferedZipper l z' r
        Nothing -> case Array.uncons r of
            Nothing -> pure Nothing
            Just { head, tail } -> runMaybeT do
                Tuple b buff <- MaybeT <<< run <<< dropLeft $ ZipperM.insertRight (uncached head) z
                buff' <- MaybeT <<< run $ ZipperM.next buff
                pure $ BufferedZipper (Array.cons (MCache.run $ MCache.evict b) l) buff' tail

-- TODO drop constraint to Applicative
prev :: forall m a. Monad m => BufferedZipper m a -> m (Maybe (BufferedZipper m a))
prev (BufferedZipper l z r) = do
    mz' <- (run $ ZipperM.prev z)
    case mz' of
        Just z' -> pure <<< Just $ BufferedZipper l z' r
        Nothing -> case Array.uncons l of
            Nothing -> pure Nothing
            Just { head, tail } -> runMaybeT do
                Tuple b buff <- MaybeT <<< run <<< dropRight $ ZipperM.insertLeft (uncached head) z
                buff' <- MaybeT <<< run $ ZipperM.prev buff
                pure $ BufferedZipper tail buff' (Array.cons (MCache.run $ MCache.evict b) r)

nextT :: forall m a. Monad m => BufferedZipper m a -> MaybeT m (BufferedZipper m a)
nextT = MaybeT <<< next

prevT :: forall m a. Monad m => BufferedZipper m a -> MaybeT m (BufferedZipper m a)
prevT = MaybeT <<< next

focus :: forall m a. BufferedZipper m a -> a
focus (BufferedZipper _ z _) = ZipperM.focus z

toArray :: forall m a. Applicative m => BufferedZipper m a -> Array (m a)
toArray (BufferedZipper l z r) =
    (Array.reverse l)
    <> map run (ZipperM.toArray z)
    <> r

-- TODO delete this in favor of ZipperM.insertLeft
snocLeft :: forall m a. m a -> ZipperM m a -> ZipperM m a
snocLeft mx (ZipperM l z r) = ZipperM (List.snoc l mx) z r

-- TODO delete this in favor of ZipperM.insertRight
snocRight :: forall m a. ZipperM m a -> m a -> ZipperM m a
snocRight (ZipperM l z r) mx = ZipperM l z (List.snoc r mx)

-- TODO move to an internal module
dropLeft :: forall m a. Applicative m => ZipperM m a -> m (Maybe (Tuple (m a) (ZipperM m a)))
dropLeft (ZipperM l z r) = case List.last l of
    Just x -> pure <<< Just $ Tuple x (ZipperM (init' l) z r)
    Nothing -> case List.head r of
        Just x -> map (\y -> Just $ Tuple (pure z) (ZipperM nil y (tail' r))) x
        Nothing -> pure Nothing

-- TODO move to an internal module
-- TODO implement it better than O(2n)
dropRight :: forall m a. Applicative m => ZipperM m a -> m (Maybe (Tuple (m a) (ZipperM m a)))
dropRight (ZipperM l z r) = case List.last r of
    Just x -> pure <<< Just $ Tuple x (ZipperM l z (init' r))
    Nothing -> case List.head l of
        Just x -> map (\y -> Just $ Tuple (pure z) (ZipperM (tail' l) y nil)) x
        Nothing -> pure Nothing

-- TODO remove this or move to an internal function
instance showBufferedZipper :: (Show (m a), Show a, Applicative m) => Show (BufferedZipper m a) where
    show (BufferedZipper l (ZipperM bl z br) r) =
        show l
        <> " |b| "
        <> show (Array.reverse <<< List.toUnfoldable $ run <$> bl)
        <> " -> "
        <> show z
        <> " <- "
        <> show (run <$> List.toUnfoldable br :: Array (m a))
        <> " |b| "
        <> show r
