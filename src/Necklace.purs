module Data.Necklace where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.HashMap (HashMap)
import Data.HashMap as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, head, tail)
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
insertRight y (Necklace { k, v, n, p } m iMax) = 
    let yKey = iMax+1
        yEntry =
            if M.size m == 1
            then { k: yKey, v: y, p: k, n: k }
            else { k: yKey, v: y, p: k, n: n }
        entry =
            if M.size m == 1
            then { k: k, v: v, p: n, n: yKey }
            else { k: k, v: v, p: p, n: yKey }
        m' = M.insert yKey yEntry <<< M.insert k entry $ m
    in
        Necklace entry m' yKey

insertLeft :: forall a. a -> Necklace a -> Necklace a
insertLeft y (Necklace { k, v, n, p } m iMax) = 
    let yKey = iMax+1
        yEntry =
            if M.size m == 1
            then { k: yKey, v: y, p: k, n: k }
            else { k: yKey, v: y, p: p, n: k }
        entry =
            if M.size m == 1
            then { k: k, v: v, p: n, n: yKey }
            else { k: k, v: v, p: yKey, n: n }
        m' = M.insert yKey yEntry <<< M.insert k entry $ m
    in
        Necklace entry m' yKey

-- if this is implemented correctly, the lookup will never return Nothing.
-- TODO write a prop test to ensure this ^^
next :: forall a. Necklace a -> Necklace a
next x@(Necklace entry m i) = fromMaybe x $ do
    v <- M.lookup entry.n m
    pure (Necklace v m i)

-- if this is implemented correctly, the lookup will never return Nothing.
-- TODO write a prop test to ensure this ^^
prev :: forall a. Necklace a -> Necklace a
prev x@(Necklace entry m i) = fromMaybe x $ do
    v <- M.lookup entry.p m
    pure (Necklace v m i)

size :: forall a. Necklace a -> Int
size (Necklace _ m _) = M.size m

toUnfoldable1 :: forall f a. Unfoldable1 f => Necklace a -> f a
toUnfoldable1 xs = unfoldr1 f (Tuple (size xs) xs)
    where
    f :: Tuple Int (Necklace a) -> Tuple a (Maybe (Tuple Int (Necklace a)))
    f (Tuple i ys) =
        if i <= 1
        then Tuple (focus ys) Nothing 
        else Tuple (focus ys) (Just (Tuple (i-1) (next ys)))
