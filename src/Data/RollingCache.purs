module Data.RollingCache where

import Prelude

import Control.Alternative ((<|>))
import Data.Array as Array
import Data.HashMap (HashMap)
import Data.HashMap as M
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Ord as Ord
import Data.Tuple (Tuple(..))


-- TODO add unit tests for this module
-- TODO put constructor in internal module and re-export type
-- | a data type for caching a fixed or unlimited number of elements.
-- | for fixed caches, when the cache overflows, it will evict the least-recently touched element.
data RollingCache k v = RollingCache (Maybe Int) (Array k) (HashMap k v)

-- | constuctor for an empty cache with the designated maximum elements.
-- | a limit of `Nothing` will create an unlimited cache.
mkRollingCache :: forall k v. Hashable k => Maybe Int -> RollingCache k v
mkRollingCache max = RollingCache max [] M.empty

-- | create a cache from an existing HashMap. If the map is bigger than the proided
-- | cache size, elements will be evicted till it reaches the correct size. No order to
-- | these evictions is guaranteed.
fromHashMap :: forall k v. Hashable k => Maybe Int -> HashMap k v -> RollingCache k v
fromHashMap max m = shrink (RollingCache max (M.keys m) m)

insert :: forall k v. Hashable k => k -> v -> RollingCache k v -> RollingCache k v
insert k v (RollingCache max ks m) =
    let m' = M.insert k v m
        ks' = touch k ks
    in shrink (RollingCache max ks' m')

-- | gets an element from the RollingCache if it does not exist or update its position to
-- | the most recently touched. Process the effects of the last element if the maximum size is exceeded
lookup :: forall k v. Hashable k => k -> RollingCache k v -> Tuple (Maybe v) (RollingCache k v)
lookup k (RollingCache max ks m) =
    let v = M.lookup k m
        ks' = if isJust v then touch k ks else ks
    in Tuple v (shrink (RollingCache max ks' m))

-- | immediately removes the element from the cache
evict :: forall k v. Hashable k => k -> RollingCache k v -> RollingCache k v
evict k (RollingCache max ks m) = RollingCache max (Array.delete k ks) (M.delete k m)

-- | immediately removes all elements from the cache including pinned elements
evictAll :: forall k v. Hashable k => RollingCache k v -> RollingCache k v
evictAll (RollingCache max _ _) = RollingCache max [] M.empty

-- | remove the least-recently touched element from the cache
pop :: forall k v. Hashable k => RollingCache k v -> RollingCache k v
pop (RollingCache max ks m) = fromMaybe (RollingCache max [] m) do
    k <- Array.last ks
    let ks' = Array.dropEnd 1 ks
    let m' = M.delete k m
    pure $ RollingCache max ks' m'

-- | remove the least-recently touched elements until the cache is within its maximum size
shrink :: forall k v. Hashable k => RollingCache k v -> RollingCache k v
shrink input@(RollingCache Nothing _ _) = input
shrink input@(RollingCache (Just max) ks _)
    | Array.length ks <= max = input
    | otherwise = shrink (pop input)

-- | similar to insert, however it this also pins an element to the cache so it will not be evicted by the rolling effect.
-- | Fails if the cache is at its max size with all pinned values already.
pin :: forall k v. Hashable k => k -> v -> RollingCache k v -> Maybe (RollingCache k v)
pin k v (RollingCache Nothing ks m) =
    Just $ RollingCache Nothing (Array.delete k ks) (M.insert k v m)
pin k v (RollingCache (Just max) ks m) = 
    if Array.null ks && M.size m >= max
    then Nothing
    else Just $ RollingCache (Just max) (Array.delete k ks) (M.insert k v m)

-- | unpins an element, but does not evict it from the cache. after unpinning,
-- | it will be the most-recently touched element in the cache. If the key was
-- | never pinned, the same cache is returned. 
unpin :: forall k v. Hashable k => k -> RollingCache k v -> RollingCache k v
unpin k input@(RollingCache max ks m) =
    if M.member k m
    then RollingCache (map (_ + 1) max) (Array.cons k ks) m
    else input

size :: forall k v. Hashable k => RollingCache k v -> Int
size (RollingCache _ _ m) = M.size m

-- TODO move to internal module
-- | moves the element to the front of the list if it exists, or prepends it otherwise.
-- | Used to update the order of cache eviction.
touch :: forall a. Eq a => a -> Array a -> Array a
touch x = Array.cons x <<< Array.delete x
