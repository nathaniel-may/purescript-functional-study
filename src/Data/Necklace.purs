module Data.Necklace where

import Prelude

import Control.Comonad (class Comonad, class Extend)
import Data.Foldable (class Foldable, foldl)
import Data.HashMap (HashMap)
import Data.HashMap as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, head, tail)
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)


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
    eq (Necklace entry m _) (Necklace entry' m' _) =
        entry == entry' && m == m'

instance foldableNecklace :: Foldable Necklace where
    foldr :: forall a b. (a -> b -> b) -> b -> Necklace a -> b
    foldr f z xs@(Necklace { k: k, v: _, n: _, p: _ } _ _) = foldr' k f z xs
        where
        foldr' start f' z' xs'@(Necklace { k: _, v: _, n: n', p: _ } _ _) =
            if start == n'
            then z'
            else f (focus xs') (foldr' start f' z' (next xs'))

    foldl :: forall a b. (b -> a -> b) -> b -> Necklace a -> b
    foldl f z xs@(Necklace { k: k, v: _, n: _, p: _ } _ _) = foldl' k f z xs
        where
        foldl' start f' z' xs'@(Necklace { k: _, v: _, n: n', p: _ } _ _) =
            if start == n'
            then z'
            else foldl' start f' (f' z' (focus xs')) (next xs')

    -- TODO make this effecient
    foldMap :: forall a m. Monoid m => (a -> m) -> Necklace a -> m
    foldMap f xs = foldl append mempty (map f xs)

-- TODO add instance for Foldable1

instance functorNecklace :: Functor Necklace where
    map f (Necklace entry m sz) = Necklace (f' entry) (map f' m) sz
        where
        f' someEntry@{ k: _, v: v, n: _, p: _ } = someEntry { v = f v }

instance extractNecklace :: Extend Necklace where
    extend :: forall b a. (Necklace a -> b) -> Necklace a -> Necklace b
    extend f = map f <<< duplicate
        where
        duplicate :: Necklace a -> Necklace (Necklace a)
        duplicate xs@(Necklace {k: k, v: _, n: _, p: _} _ _) =
            go k (next xs) (singleton xs)
        
        go :: Int -> Necklace a -> Necklace (Necklace a) -> Necklace (Necklace a)
        go start xs@(Necklace {k: k, v: _, n: _, p: _} _ _) xxs = 
            if k == start
            then next xxs
            else go start (next xs) (next $ insertRight xs xxs)

instance comonadNecklace :: Comonad Necklace where
    extract :: forall a. Necklace a -> a
    extract = focus

instance arbitraryNecklace :: Arbitrary a => Arbitrary (Necklace a) where
    arbitrary = fromNonEmpty <$> (arbitrary :: Gen (NonEmpty Array a))

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
insertRight newValue old@(Necklace focusEntry@{ k, v, n, p } m maxKey) = 
    fromMaybe old $ do
        -- make the entry for the new value
        let newKey = maxKey + 1
        let newEntry = { k: newKey, v: newValue, p: k, n: n }

        -- update the n pointer for the element in focus in the necklace
        let focusEntry' = 
                -- if the necklace is a singleton, handle the loop around
                if size old == 1
                then (focusEntry { n=newKey, p=newKey })
                else (focusEntry { n=newKey })

        -- update the p pointer for the next element in the necklace
        nEntry' <- (_ { p=newKey }) <$> M.lookup n m

        -- update the map with all 3 entrires
        let m' = M.insert k focusEntry'
                <<< M.insert newKey newEntry
                <<< M.insert n nEntry' $ m

        -- make the necklace
        pure $ Necklace focusEntry' m' newKey

insertLeft :: forall a. a -> Necklace a -> Necklace a
insertLeft newValue old@(Necklace focusEntry@{ k, v, n, p } m maxKey) = 
    fromMaybe old $ do
        -- make the entry for the new value
        let newKey = maxKey + 1
        let newEntry = { k: newKey, v: newValue, p: p, n: k }

        -- update the n pointer for the element in focus in the necklace
        let focusEntry' =  
                -- if the necklace is a singleton, handle the loop around           
                if size old == 1
                then (focusEntry { p=newKey, n=newKey })
                else (focusEntry { p=newKey })

        -- update the n pointer for the previous element in the necklace
        pEntry' <- (_ { n=newKey }) <$> M.lookup p m

        -- update the map with all 3 entrires
        let m' = M.insert p pEntry'
                <<< M.insert newKey newEntry
                <<< M.insert k focusEntry' $ m

        -- make the necklace
        pure $ Necklace focusEntry' m' newKey

-- TODO add removeLeft, removeRight, and change "size" to maxIndex (it won't be the size anymore when you can remove elements)

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
