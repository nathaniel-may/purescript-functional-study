module Data.Zipper where

import Prelude

import Data.Identity (Identity)
import Data.Lazy (force)
import Data.List.Lazy (List)
import Data.List.Lazy.Types (NonEmptyList(..))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr1)
import Data.ZipperM (ZipperM, focus)
import Data.ZipperM as ZipperM
import ZipperM.Utils (runIdentity)
import Data.NonEmpty as NonEmpty


-- TODO implement without ZipperM
-- TODO implement Necklace and NecklaceM
type Zipper a = ZipperM Identity a

fromNonEmpty :: forall a. NonEmptyList a -> Zipper a
fromNonEmpty (NonEmptyList xs') = 
    ZipperM.fromList1 (NonEmpty.head xs) (pure <$> NonEmpty.tail xs)
    where xs = force xs'

fromList :: forall a. List a -> Maybe (Zipper a)
fromList = runIdentity <<< ZipperM.fromList <<< map pure

next :: forall a. Zipper a -> Maybe (Zipper a)
next = runIdentity <<< ZipperM.next

prev :: forall a. Zipper a -> Maybe (Zipper a)
prev = runIdentity <<< ZipperM.prev

first :: forall a. Zipper a → Zipper a
first = runIdentity <<< ZipperM.first

last :: forall a. Zipper a → Zipper a
last = runIdentity <<< ZipperM.last

toUnfoldable :: forall f. Unfoldable f => Zipper ~> f
toUnfoldable zipper = unfoldr1
    (\z -> Tuple (focus z) (next z))
    (first zipper)
