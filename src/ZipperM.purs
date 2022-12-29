module Data.ZipperM where

import Prelude

import Control.Extend (class Extend)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.List.Lazy (List, nil, (:))
import Data.List.Lazy as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable1, unfoldr1)
import ZipperM.Utils (init', tail')


-- TODO swap Lazy Lists for buffers that can drop from the back. Maybe use Data.Sequence?
data ZipperM m a = ZipperM (List (m a)) a (List (m a))

instance unfoldable1ZipperM :: Applicative m => Unfoldable1 (ZipperM m) where
    unfoldr1 :: forall a b. (b -> Tuple a (Maybe b)) -> b -> ZipperM m a
    unfoldr1 f z = case f z of
        Tuple x Nothing -> ZipperM nil x nil
        Tuple x (Just y) -> ZipperM nil x (pure <$> unfoldr1 f y)

-- instance functorZipperM :: Monad m => Functor (ZipperM m) where
--     map f zipper = toUnfoldable ... TODO

-- instance extendZipperM :: Extend (ZipperM m) where
--     extend :: forall b a. (ZipperM m a -> b) -> ZipperM m a -> ZipperM m b
--     extend f x = ZipperM nil (f x) nil

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

-- | moves the focus of the zipper to the first element
first :: forall m a. Monad m => ZipperM m a -> m (ZipperM m a)
first zipper = maybe (pure zipper) first =<< prev zipper

-- | moves the focus of the zipper to the last element
last :: forall m a. Monad m => ZipperM m a -> m (ZipperM m a)
last zipper = maybe (pure zipper) last =<< next zipper

toList :: forall m a. Applicative m => ZipperM m a -> List (m a)
toList (ZipperM l z r) = l <> (pure z : nil) <> r

toArray :: forall m a. Applicative m => ZipperM m a -> Array (m a)
toArray = List.toUnfoldable <<< toList

-- TODO should this be part of an UnfoldableM1 class?
unfoldM1 :: forall m a b. Monad m => (b -> m (Tuple a (Maybe b))) -> b -> m (ZipperM m a)
unfoldM1 f z = do
    Tuple x my <- f z
    case my of
        Nothing -> pure $ ZipperM nil x nil
        Just y -> ZipperM nil x <<< map pure <$> go f y
    where 
    go :: (b -> m (Tuple a (Maybe b))) -> b -> m (List a)
    go f' z' = do
        Tuple x my <- f' z'
        case my of
            Nothing -> pure nil
            Just y -> List.cons x <$> go f' y
