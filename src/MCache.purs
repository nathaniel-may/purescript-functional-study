module Data.MCache where

import Prelude

import Control.Comonad (class Comonad, class Extend, extract)
import Data.Maybe (Maybe(..))


-- | A datatype for memoizing and evicting data. Useful as a unit of a buffer.
data MCache m a = MCache (m a) (Maybe a)

instance functorMCache :: Functor m => Functor (MCache m) where
    map f (MCache mx my) = MCache (map f mx) (map f my)

instance applyMCache :: Apply m => Apply (MCache m) where
    apply :: forall a b. MCache m (a -> b) -> MCache m a -> MCache m b
    apply (MCache _ (Just f)) (MCache mx my) = MCache (f <$> mx) (f <$> my)
    apply (MCache mf Nothing) (MCache mx _) = MCache (mf <*> mx) Nothing

instance applicativeMCache :: Applicative m => Applicative (MCache m) where
    pure x = MCache (pure x) (Just x)

instance extendMCacheW :: Functor w => Extend (MCache w) where
    extend :: forall b a. (MCache w a -> b) -> MCache w a -> MCache w b
    extend f cache@(MCache wx _) = let y = f cache
        in MCache (const y <$> wx) (Just y)

instance comonadMCacheW :: Comonad w => Comonad (MCache w) where
    extract :: forall a. MCache w a -> a
    extract (MCache wx Nothing) = extract wx
    extract (MCache _ (Just x)) = x

-- TODO is this possible to describe?
-- instance bindMCacheM :: Bind m => Bind (m (MCache m a)) where

run :: forall m a. Applicative m => MCache m a -> m a
run (MCache _ (Just x)) = pure x
run (MCache mx Nothing) = mx

force :: forall m a. Applicative m => MCache m a -> m (MCache m a)
force (MCache mx Nothing) = (\x -> MCache mx $ Just x) <$> mx
force cache = pure cache

evict :: forall m a. Applicative m => MCache m a -> MCache m a
evict (MCache mx (Just _)) = MCache mx Nothing
evict cache = cache
