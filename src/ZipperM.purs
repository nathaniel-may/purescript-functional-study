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

import Data.List.Lazy (List, nil, (:))
import Data.List.Lazy as List
import Data.Maybe (Maybe, fromMaybe)
import ZipperM.Utils (init', tail')


data ZipperM m a = ZipperM (List (m a)) a (List (m a))

mkZipperM :: forall m a. a -> List (m a) -> ZipperM m a
mkZipperM = ZipperM nil

mkZipperM' :: forall m a. Applicative m => List (m a) -> Maybe (m (ZipperM m a))
mkZipperM' xs = map (\z -> ZipperM nil z xs) <$> List.head xs

next :: forall m a. Applicative m => ZipperM m a -> Maybe (m (ZipperM m a))
next (ZipperM left z right) =
    map (\z' -> ZipperM (left `List.snoc` pure z) z' (tail' right)) <$> List.head right

next' :: forall m a. Applicative m => ZipperM m a -> m (ZipperM m a)
next' zipper = fromMaybe (pure zipper) (next zipper)

prev :: forall m a. Applicative m => ZipperM m a -> Maybe (m (ZipperM m a))
prev (ZipperM left z right) =
    map (\z' -> ZipperM (init' left) z' (pure z `List.cons` right)) <$> List.last left

prev' :: forall m a. Applicative m => ZipperM m a -> m (ZipperM m a)
prev' zipper = fromMaybe (pure zipper) (prev zipper)

focus :: forall m a. ZipperM m a -> a
focus (ZipperM _ z _) = z

toList :: forall m a. Applicative m => ZipperM m a -> List (m a)
toList (ZipperM l z r) = l <> (pure z : nil) <> r

toArray :: forall m a. Applicative m => ZipperM m a -> Array (m a)
toArray = List.toUnfoldable <<< toList
