module Data.BufferedZipper where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.State (State, runState)
import Data.Array as Array
import Data.List.Lazy (nil)
import Data.List.Lazy as List
import Data.MCache (MCache, force, run, uncached)
import Data.MCache as MCache
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Zipper (Zipper(..))
import Data.Zipper as Zipper
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Utils (init', tail')


data BufferedZipper m a = BufferedZipper 
    (Array (m a))
    (Zipper (MCache m a))
    (Array (m a))
    a

instance eqZipperM :: (Applicative m, Eq (m a), Eq a) => Eq (BufferedZipper m a) where
    eq z z' = toArray z == toArray z'

instance functorBufferedZipper :: Functor m => Functor (BufferedZipper m) where
    map f (BufferedZipper l buff r z) = BufferedZipper
        (map (map f) l)
        (map (map f) buff)
        (map (map f) r)
        (f z)

-- TODO double check this?
-- BufferedZipper is not a comonad because extend cannot be implemented for arbitrary effects

instance arbitraryZipperM :: (Arbitrary (m a), Arbitrary a, Applicative m) => Arbitrary (BufferedZipper m a) where
    arbitrary = do
        x <- arbitrary
        xs <- arbitrary :: Gen (Array a)
        ys <- arbitrary
        let buffer = Zipper.fromNonEmpty $ pure <$> (x :| List.fromFoldable xs)
        pure $ BufferedZipper [] buffer ys x

-- | buffersize must be at least 1.
-- | For no buffer, use `Zipper` and for an infinite buffer use `ZipperM`
-- TODO use NonEmpty Array (m a)?
mkBufferedZipper :: forall m a. Applicative m => Int -> Array (m a) -> m (Maybe (BufferedZipper m a))
mkBufferedZipper bsize xs
    | bsize < 1 = pure Nothing
    | otherwise = sequence $ (\{ head, tail } ->
        let { before, after } = Array.splitAt (bsize - 1) tail
        in map (\h -> BufferedZipper [] (Zipper.fromNonEmpty (pure h :| (List.fromFoldable $ map uncached before))) after h) head
    ) <$> (Array.uncons xs)
    
-- TODO drop constraint to Applicative
next :: forall m a. Monad m => BufferedZipper m a -> m (Maybe (BufferedZipper m a))
next (BufferedZipper l buff r _) = case Zipper.next buff of
    Just buff' -> map Just $ run =<< (force $ map (\z -> BufferedZipper l buff' r z) (Zipper.focus buff'))
    Nothing -> case Array.uncons r of
        Nothing -> pure Nothing
        Just { head, tail } -> runMaybeT do
            Tuple b buff' <- MaybeT <<< pure $ dropLeftAndNext $ Zipper.insertRight (uncached head) buff
            let l' = Array.cons (MCache.run $ MCache.evict b) l
            let mbz' = map (\z -> BufferedZipper l' buff' tail z) (Zipper.focus buff')
            MaybeT <<< map Just $ run =<< (force mbz')

-- TODO drop constraint to Applicative
prev :: forall m a. Monad m => BufferedZipper m a -> m (Maybe (BufferedZipper m a))
prev (BufferedZipper l buff r _) = case Zipper.prev buff of
    Just buff' -> map Just $ run =<< (force $ map (\z -> BufferedZipper l buff' r z) (Zipper.focus buff'))
    Nothing -> case Array.uncons l of
        Nothing -> pure Nothing
        Just { head, tail } -> runMaybeT do
            Tuple b buff' <- MaybeT <<< pure $ dropRightAndPrev $ Zipper.insertLeft (uncached head) buff
            let r' = Array.cons (MCache.run $ MCache.evict b) r
            let mbz' = map (\z -> BufferedZipper tail buff' r' z) (Zipper.focus buff')
            MaybeT <<< map Just $ run =<< (force mbz')

nextT :: forall m a. Monad m => BufferedZipper m a -> MaybeT m (BufferedZipper m a)
nextT = MaybeT <<< next

prevT :: forall m a. Monad m => BufferedZipper m a -> MaybeT m (BufferedZipper m a)
prevT = MaybeT <<< prev

focus :: forall m a. BufferedZipper m a -> a
focus (BufferedZipper _ _ _ z) = z

toArray :: forall m a. Applicative m => BufferedZipper m a -> Array (m a)
toArray (BufferedZipper l buff r _) =
    (Array.reverse l)
    <> map run (Zipper.toUnfoldable buff)
    <> r

-- TODO delete this in favor of ZipperM.insertLeft
snocLeft :: forall m a. m a -> Zipper (m a) -> Zipper (m a)
snocLeft mx (Zipper l z r) = Zipper (List.snoc l mx) z r

-- TODO delete this in favor of ZipperM.insertRight
snocRight :: forall m a. Zipper (m a) -> m a -> Zipper (m a)
snocRight (Zipper l z r) mx = Zipper l z (List.snoc r mx)

-- TODO move to an internal module
dropLeftAndNext :: forall m a. Applicative m => Zipper (m a) -> Maybe (Tuple (m a) (Zipper (m a)))
dropLeftAndNext (Zipper l z r) = case List.last l of
    -- since the focus isn't being dropped, move the zipper as well
    Just x -> map (\y -> Tuple x y ) (Zipper.next $ Zipper (init' l) z r)
    Nothing -> case List.head r of
        -- we have to drop the focus so it's already being forced to move
        Just x -> Just $ Tuple z (Zipper nil x (tail' r))
        Nothing -> Nothing

-- TODO move to an internal module
-- TODO implement it better than O(2n) (put unsnoc in Utils?)
dropRightAndPrev :: forall m a. Applicative m => Zipper (m a) -> Maybe (Tuple (m a) (Zipper (m a)))
dropRightAndPrev (Zipper l z r) = case List.last r of
    -- since the focus isn't being dropped, move the zipper as well
    Just x -> map (\y -> Tuple x y ) (Zipper.prev $ Zipper l z (init' r))
    Nothing -> case List.head l of
        -- we have to drop the focus so it's already being forced to move
        Just x -> Just $ Tuple z (Zipper (tail' l) x nil)
        Nothing -> Nothing

-- TODO remove this or move to an internal function
instance showBufferedZipper :: (Show (m a), Show a, Applicative m) => Show (BufferedZipper m a) where
    show (BufferedZipper l (Zipper bl _ br) r z) =
        show l
        <> " |b| "
        <> show (Array.reverse <<< List.toUnfoldable $ run <$> bl)
        <> " -> "
        <> show z
        <> " <- "
        <> show ((List.toUnfoldable $ run <$> br) :: Array (m a))
        <> " |b| "
        <> show r

debugPrint :: forall a. Show a => BufferedZipper (State Int) a -> String
debugPrint (BufferedZipper l (Zipper bl _ br) r z) =
    show (debugState <$> l)
    <> " |b| "
    <> show (Array.reverse <<< List.toUnfoldable $ debugState <<< MCache.run <$> bl)
    <> " -> "
    <> show z
    <> " <- "
    <> show (List.toUnfoldable $ debugState <<< MCache.run <$> br :: Array String)
    <> " |b| "
    <> show (debugState <$> r)

debugState :: forall a. Show a => State Int a -> String
debugState m = case runState m 0 of
    Tuple a s -> "{" <> show s <> "|" <> show a <> "}"

-- import Prelude
-- import Data.Array as Array 
-- import Data.BufferedZipper 
-- import Data.Functor (voidLeft) 
-- import Control.Monad.State (State, evalState, execState, runState, modify, get)
-- import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
-- import Data.Tuple (Tuple(..))
-- import Data.Zipper (Zipper(..))
-- import Data.List.Lazy as List 
-- import Data.MCache as MCache
-- mz = mkBufferedZipper 2 (voidLeft (modify (_ + 1)) <$> ['a', 'b', 'c', 'd'] :: Array (State Int Char))
-- runState (runMaybeT $ debugPrint <$> (nextT =<< MaybeT mz)) 0 