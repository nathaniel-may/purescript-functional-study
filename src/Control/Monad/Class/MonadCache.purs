module Control.Monad.Cache.Class where

import Prelude

import Data.HashMap (HashMap)
import Data.HashMap as M
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))


class (Hashable k, Monad m) <= MonadCache m k v where
    cache :: forall a. ((HashMap k v) -> (Tuple a (HashMap k v))) -> m a

insert :: forall m k v. MonadCache m k v => k -> v -> m Unit
insert k v = cache \m -> Tuple unit (M.insert k v m)

retrieve :: forall m k v. MonadCache m k v => k -> m (Maybe v)
retrieve k = cache \m -> Tuple (M.lookup k m) m

evict :: forall m k v. MonadCache m k v => k -> m Unit
evict k = cache \m -> Tuple unit (M.delete k m :: HashMap k v)

evictAll :: forall m k v. MonadCache m k v => m Unit
evictAll = cache \_ -> Tuple unit (M.empty :: HashMap k v)

-- | gets the value using the cache if possible,
-- | otherwise it runs the effect and puts it in the cache
fetch :: forall m k v. MonadCache m k v => (k -> m v) -> k -> m v
fetch f k = retrieve k >>= case _ of
    Nothing -> f k
    Just v -> insert k v $> v
