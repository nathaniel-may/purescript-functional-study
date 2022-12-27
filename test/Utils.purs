module ZipperM.Test.Utils where

import Prelude

import Data.Array (cons, uncons)
import Data.Maybe (Maybe(..))
import Data.ZipperM (ZipperM, focus, next, next', prev, prev')

data PN = P | N

type ZipperPath = Array PN

-- | walk a zipper with a specific path. The Zipper's focus is the first element.
-- | If the path walks off the zipper, the path up till that point will be returned.
walk :: forall m a. Monad m => ZipperPath -> ZipperM m a -> m (Array a)
walk path zipper = case uncons path of
    Nothing -> pure [focus zipper]
    Just { head, tail } -> 
        let
            mz = case head of
                P -> prev zipper
                N -> next zipper
        in
            case mz of
                Nothing -> pure [focus zipper]
                Just zipper' -> cons (focus zipper) <$> (walk tail =<< zipper')

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
