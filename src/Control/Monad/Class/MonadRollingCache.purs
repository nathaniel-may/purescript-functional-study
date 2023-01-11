module Control.Monad.RollingCache.Class where

import Prelude

import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.RollingCache (RollingCache)
import Data.RollingCache as Cache
import Data.Tuple (Tuple(..))


class (Hashable k, Monad m) <= MonadRollingCache k v m where
    cache :: forall a. (RollingCache k v -> (Tuple a (RollingCache k v))) -> m a

insert :: forall m k v. MonadRollingCache k v m => k -> v -> m Unit
insert k v = cache \c -> Tuple unit (Cache.insert k v c)

-- TODO should I rename this `lookup`?
-- | retrieve a value from the cache by its key 
retrieve :: forall m k v. MonadRollingCache k v m => k -> m (Maybe v)
retrieve k = cache (Cache.lookup k)

-- | retrieve a value that depends on the entire cache
retrieves :: forall m k v a. MonadRollingCache k v m => (RollingCache k v -> a) -> m a
retrieves f = cache \c -> Tuple (f c) c

evict :: forall m k v. MonadRollingCache k v m => k -> m Unit
evict k = cache \c -> Tuple unit (Cache.evict k c :: RollingCache k v)

evictAll :: forall m k v. MonadRollingCache k v m => m Unit
evictAll = cache \c -> Tuple unit (Cache.evictAll c :: RollingCache k v)

-- | gets the value using the cache if possible,
-- | otherwise it runs the effect and puts it in the cache
fetch :: forall m k v. MonadRollingCache k v m => (k -> m v) -> k -> m v
fetch f k = retrieve k >>= case _ of
    Nothing -> f k >>= \v -> insert k v $> v
    Just v -> pure v

size :: forall m k v. MonadRollingCache k v m => m Int
size = retrieves (Cache.size :: RollingCache k v -> Int)
