module ZipperM.Test.Utils where

import Prelude

import Data.Array (cons, uncons)
import Data.BufferedZipper (BufferedZipper)
import Data.BufferedZipper as BZ
import Data.MCache (MCache(..), force)
import Data.Maybe (Maybe(..))
import Data.ZipperM (ZipperM, focus, next, next', prev, prev')
import Test.QuickCheck (class Arbitrary, arbitrary)


data PN = P | N

type ZipperPath = Array PN

newtype MCacheM m a = MCacheM (m (MCache m a))

instance arbitraryMCacheM :: (Applicative m, Arbitrary (m a)) => Arbitrary (MCacheM m a) where
    arbitrary = do
        m <- arbitrary
        b <- arbitrary
        let cache = MCache m Nothing
        pure <<< MCacheM $ if b then pure cache else force cache

-- | walk a zipper with a specific path. The Zipper's focus is the first element.
-- | If the path walks off the zipper, the path up till that point will be returned.
walk :: forall m a. Monad m => ZipperPath -> ZipperM m a -> m (Array a)
walk path zipper = case uncons path of
    Nothing -> pure [focus zipper]
    Just { head, tail } -> do
        mz <- case head of
                P -> prev zipper
                N -> next zipper
        case mz of
                Nothing -> pure [focus zipper]
                Just zipper' -> cons (focus zipper) <$> (walk tail zipper')

-- | Walk a zipper with a specific path. The Zipper's focus is the first element.
-- | If the path walks off the zipper, a repeat of the focus will be added till the path
-- | walks back on the zipper.
walk' :: forall m a. Monad m => ZipperPath -> ZipperM m a -> m (Array a)
walk' path zipper = case uncons path of
    Nothing -> pure [focus zipper]
    Just { head, tail } ->
        let
            zipper' = case head of 
                P -> prev' zipper
                N -> next' zipper
        in
            cons (focus zipper) <$> (walk' tail =<< zipper')

-- | walk a BufferedZipper with a specific path. The Zipper's focus is the first element.
-- | If the path walks off the zipper, the path up till that point will be returned.
bWalk :: forall m a. Monad m => ZipperPath -> BufferedZipper m a -> m (Array a)
bWalk path zipper = case uncons path of
    Nothing -> pure [BZ.focus zipper]
    Just { head, tail } -> do
        mz <- case head of
                P -> BZ.prev zipper
                N -> BZ.next zipper
        case mz of
                Nothing -> pure [BZ.focus zipper]
                Just zipper' -> cons (BZ.focus zipper) <$> (bWalk tail zipper')