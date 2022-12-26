module Data.ZipperM (
    ZipperM -- not exporting constructor
    , focus
    , mkZipperM
    , mkZipperM'
    , next
    , next'
    , prev
    , prev'
    , toArray
) where

import Prelude

import Data.Array (cons, snoc)
import Data.Array as Array
import Data.Maybe (Maybe, fromMaybe)
import ZipperM.Utils (init', tail')


data ZipperM m a = ZipperM (Array (m a)) a (Array (m a))

mkZipperM :: forall m a. a -> Array (m a) -> ZipperM m a
mkZipperM = ZipperM []

mkZipperM' :: forall m a. Applicative m => Array (m a) -> Maybe (m (ZipperM m a))
mkZipperM' xs = map (\z -> ZipperM [] z xs) <$> Array.head xs

next :: forall m a. Applicative m => ZipperM m a -> Maybe (m (ZipperM m a))
next (ZipperM left z right) =
    map (\z' -> ZipperM (left `snoc` pure z) z' (tail' right)) <$> Array.head right

next' :: forall m a. Applicative m => ZipperM m a -> m (ZipperM m a)
next' zipper = fromMaybe (pure zipper) (next zipper)

prev :: forall m a. Applicative m => ZipperM m a -> Maybe (m (ZipperM m a))
prev (ZipperM left z right) =
    map (\z' -> ZipperM (init' left) z' (pure z `cons` right)) <$> Array.last left

prev' :: forall m a. Applicative m => ZipperM m a -> m (ZipperM m a)
prev' zipper = fromMaybe (pure zipper) (prev zipper)

focus :: forall m a. ZipperM m a -> a
focus (ZipperM _ z _) = z

toArray :: forall m a. Applicative m => ZipperM m a -> Array (m a)
toArray (ZipperM l z r) = l <> [pure z] <> r
