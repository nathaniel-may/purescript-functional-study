module Data.ZipperM (
    ZipperM -- not exporting constructor
    , focus
    , mkZipperM
    , mkZipperM'
    , next
    , next'
    , nextT
    , prev
    , prev'
    , prevT
    , toArray
) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.List.Lazy (List, nil, (:))
import Data.List.Lazy as List
import Data.Maybe (Maybe, fromMaybe)
import Data.Traversable (sequence)
import ZipperM.Utils (init', tail')


-- TODO swap Lazy Lists for buffers that can drop from the back. Maybe use Data.Sequence?
data ZipperM m a = ZipperM (List (m a)) a (List (m a))

mkZipperM :: forall m a. a -> List (m a) -> ZipperM m a
mkZipperM = ZipperM nil

mkZipperM' :: forall m a. Applicative m => List (m a) -> m (Maybe (ZipperM m a))
mkZipperM' xs = map (\z -> ZipperM nil z xs) <$> (sequence $ List.head xs)

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

toList :: forall m a. Applicative m => ZipperM m a -> List (m a)
toList (ZipperM l z r) = l <> (pure z : nil) <> r

toArray :: forall m a. Applicative m => ZipperM m a -> Array (m a)
toArray = List.toUnfoldable <<< toList
