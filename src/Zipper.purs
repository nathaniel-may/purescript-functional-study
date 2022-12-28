module Data.Zipper where

import Prelude

import Data.Identity (Identity)
import Data.List.Lazy (List)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr1)
import Data.ZipperM (ZipperM, focus, mkZipperM, mkZipperM')
import Data.ZipperM as ZipperM
import ZipperM.Utils (runIdentity)


type Zipper a = ZipperM Identity a

mkZipper :: forall a. a -> List a -> Zipper a
mkZipper x = mkZipperM x <<< map pure

mkZipper' :: forall a. List a -> Maybe (Zipper a)
mkZipper' = runIdentity <<< mkZipperM' <<< map pure

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
