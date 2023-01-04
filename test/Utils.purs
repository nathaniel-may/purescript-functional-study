module ZipperM.Test.Utils where

import Prelude

import Data.Array (cons, uncons)
import Data.BufferedZipper (BufferedZipper)
import Data.BufferedZipper as BZ
import Data.MCache (MCache(..), force)
import Data.Maybe (Maybe(..))
import Data.ZipperM (ZipperM)
import Data.ZipperM as ZipperM
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

walkZipperM :: forall m a. Monad m => ZipperPath -> ZipperM m a -> m (Array a)
walkZipperM = walkM ZipperM.prev ZipperM.focus ZipperM.next

walkBufferedZipper :: forall m a. Monad m => ZipperPath -> BufferedZipper m a -> m (Array a)
walkBufferedZipper = walkM BZ.prev BZ.focus BZ.next

walk ::
    forall t a
    . (t a -> Maybe (t a))
    -> (t a -> a)
    -> (t a -> Maybe (t a))
    -> ZipperPath
    -> t a
    -> Array a
walk prev focus next path xs = case uncons path of
    Nothing -> [focus xs]
    Just { head, tail } ->
        let mxs' = case head of
                P -> prev xs
                N -> next xs
        in case mxs' of
            Nothing -> [focus xs]
            Just xs' -> cons (focus xs) (walk prev focus next tail xs')

walkM ::
    forall m t a
    . Monad m
    => (t m a -> m (Maybe (t m a)))
    -> (t m a -> a)
    -> (t m a -> m (Maybe (t m a)))
    -> ZipperPath
    -> t m a
    -> m (Array a)
walkM prev focus next path xs = case uncons path of
    Nothing -> pure [focus xs]
    Just { head, tail } -> do
        mxs' <- case head of
            P -> prev xs
            N -> next xs
        case mxs' of
            Nothing -> pure [focus xs]
            Just xs' -> cons (focus xs) <$> (walkM prev focus next tail xs')
