module Data.Necklace where

import Prelude

import Control.Comonad (class Comonad, class Extend)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.HashMap (HashMap)
import Data.HashMap as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, head, tail)
import Data.Semigroup.Foldable (class Foldable1, foldr1)
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)


type Entry a =
    { v :: a
    , k :: Int
    , n :: Int
    , p :: Int
    }

-- TODO move constructor to internal module and re-export
data Necklace a = Necklace (Entry a) (HashMap Int (Entry a)) Int

-- identical Necklaces that are not in the same focus position are not equal
instance eqNecklace :: Eq a => Eq (Necklace a) where
    eq (Necklace entry m _) (Necklace entry' m') =
        entry == entry' && m == m'

-- instance foldableNecklace :: Foldable Necklace where
--     foldr :: forall a b. (a -> b -> b) -> b -> Necklace a -> b
--     foldr f z xs@(Necklace { k, v, n, p } _ _) = foldr' k f z xs
--         where
--         foldr' start f' z' xs'@(Necklace { k, v, n, p } _ _) =
--             if start == n
--             then z'
--             else f (focus xs') (foldr' start f z' (next xs'))

--     foldl :: forall a b. (b -> a -> b) -> b -> Necklace a -> b
--     foldl f z xs@(Necklace { k, v, n, p } _ _) = foldlBOOP k f z xs
--         where
--         foldlBOOP start f' z' xs'@(Necklace { k, v, n, p } _ _) =
--             if start == n
--             then z'
--             else foldlBOOP f' (f' z' (focus xs')) (next xs')

--     foldMap :: forall a m. Monoid m => (a -> m) -> Necklace a -> m
--     foldMap = 0

-- TODO add instance for Foldable1

instance functorNecklace :: Functor Necklace where
    map f (Necklace entry m sz) = Necklace (f' entry) (map f' m) sz
        where
        f' someEntry@{ k: _, v: v, n: _, p: _ } = someEntry { v = f v }

instance extractNecklace :: Extend Necklace where
    extend :: forall b a. (Necklace a -> b) -> Necklace a -> Necklace b
    extend f = map f <<< duplicate
        where
        duplicate :: forall a. Necklace a -> Necklace (Necklace a)
        duplicate xs = foldr f (singleton xs) (next xs)

        f :: forall a. Necklace a -> Necklace (Necklace a) -> Necklace (Necklace a)
        f y ys = next $ insertRight y ys

instance comonadNecklace :: Comonad Necklace where
    extract :: forall a. Necklace a -> a
    extract = focus

fromNonEmpty :: forall f a. Foldable f => NonEmpty f a -> Necklace a
fromNonEmpty xs = next $ foldl (\ys y -> next $ insertRight y ys) (singleton $ head xs) (tail xs)

singleton :: forall a. a -> Necklace a
singleton x = 
    let entry = { k: 1, v: x, n: 1, p: 1}
        m = M.singleton 1 entry
    in
        Necklace entry m 1

focus :: forall a. Necklace a -> a
focus (Necklace entry _ _) = entry.v

insertRight :: forall a. a -> Necklace a -> Necklace a
insertRight y (Necklace { k, v, n, p } m sz) = 
    let yKey = sz + 1
        yEntry =
            if sz == 1
            then { k: yKey, v: y, p: k, n: k }
            else { k: yKey, v: y, p: k, n: n }
        entry =
            if sz == 1
            then { k: k, v: v, p: yKey, n: yKey }
            else { k: k, v: v, p: p, n: yKey }
        m' = M.insert yKey yEntry <<< M.insert k entry $ m
    in
        Necklace entry m' (sz + 1)

insertLeft :: forall a. a -> Necklace a -> Necklace a
insertLeft y (Necklace { k, v, n, p } m sz) = 
    let yKey = sz + 1
        yEntry =
            if sz == 1
            then { k: yKey, v: y, p: k, n: k }
            else { k: yKey, v: y, p: p, n: k }
        entry =
            if sz == 1
            then { k: k, v: v, p: yKey, n: yKey }
            else { k: k, v: v, p: yKey, n: n }
        m' = M.insert yKey yEntry <<< M.insert k entry $ m
    in
        Necklace entry m' (sz + 1)

-- if Necklace is implemented correctly, the lookup will never return Nothing.
-- TODO write a prop test to ensure this ^^
next :: forall a. Necklace a -> Necklace a
next x@(Necklace entry m i) = fromMaybe x $ do
    v <- M.lookup entry.n m
    pure (Necklace v m i)

-- if Necklace is implemented correctly, the lookup will never return Nothing.
-- TODO write a prop test to ensure this ^^
prev :: forall a. Necklace a -> Necklace a
prev x@(Necklace entry m i) = fromMaybe x $ do
    v <- M.lookup entry.p m
    pure (Necklace v m i)

size :: forall a. Necklace a -> Int
size (Necklace _ _ x) = x

toUnfoldable1 :: forall f a. Unfoldable1 f => Necklace a -> f a
toUnfoldable1 xs = unfoldr1 f (Tuple (size xs) xs)
    where
    f :: Tuple Int (Necklace a) -> Tuple a (Maybe (Tuple Int (Necklace a)))
    f (Tuple i ys) =
        if i <= 1
        then Tuple (focus ys) Nothing 
        else Tuple (focus ys) (Just (Tuple (i-1) (next ys)))
