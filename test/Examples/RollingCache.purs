module Test.Examples.RollingCache where

import Prelude

import Control.Monad.RollingCache.Class (class MonadRollingCache)
import Control.Monad.RollingCache.Class as Cache
import Control.Monad.RollingCacheT (RollingCacheT, evalRollingCacheT)
import Data.HashMap as M
import Data.Maybe (Maybe(..))
import Data.RollingCache (RollingCache(..), mkRollingCache)
import Data.Traversable (sum, traverse)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)


main :: Effect Unit
main = runAff_ (const $ pure unit) $ runProdM debugApp (mkRollingCache $ Just 3)

newtype ProdM a = ProdM (RollingCacheT String String Aff a)

derive newtype instance functorProdM :: Functor ProdM
derive newtype instance applyProdM :: Apply ProdM
derive newtype instance applicativeProdM :: Applicative ProdM
derive newtype instance bindProdM :: Bind ProdM
derive newtype instance monadProdM :: Monad ProdM
derive newtype instance monadEffectProdM :: MonadEffect ProdM
derive newtype instance monadAffProdM :: MonadAff ProdM
derive newtype instance monadRollingCacheProdM :: MonadRollingCache String String ProdM

runProdM :: forall a. ProdM a -> RollingCache String String -> Aff a
runProdM (ProdM m) = evalRollingCacheT m

simulatedUserInput :: Array String
simulatedUserInput = ["a", "b", "c", "d", "b", "c", "a"]

simulatedNetworkCall :: String -> Aff String
simulatedNetworkCall s = case s of
    "a" -> pure "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    "b" -> pure "Curabitur scelerisque cursus placerat."
    "c" -> pure "Praesent ullamcorper mi at mi mattis, in vehicula mi volutpat."
    "d" -> pure "Aenean et eros ut justo imperdiet placerat quis at ipsum."
    _ -> pure ""


-- without a cache, the simulated user input would make 7 network calls to fetch 4 resources.
-- because we're using a rolling cache of size 3, we only make 5 network calls for the same input.
-- run `debugApp` to see cache information printed.
app :: forall m
    . MonadRollingCache String String m
    => MonadAff m
    => m Unit
app = void $ traverse f simulatedUserInput
    where
    f :: String -> m Unit
    f x = do
        -- get the value from the cache if it's there, otherwise use the function to get it over the simulated network
        resource <- Cache.fetch (liftAff <<< simulatedNetworkCall) x
        -- print the resource (will look the same regardless of origin)
        liftEffect $ log resource

debugApp :: forall m
    . MonadRollingCache String String m
    => MonadAff m
    => m Unit
debugApp = do
    effects <- traverse f simulatedUserInput
    -- TODO this count is wrong
    liftEffect <<< log $ "total effects executed: " <> show (sum effects)
    where
    f :: String -> m Int
    f x = do
        -- check if it's in the cache (not normally necessary)
        inCache <- Cache.retrieves (\(c :: RollingCache String String) -> case c of (RollingCache _ _ m) -> M.member x m)
        -- get the value from the cache if it's there, otherwise use the function to get it over the simulated network
        resource <- Cache.fetch (liftAff <<< simulatedNetworkCall) x
        -- print the resource with a marker if it is from the cache (🗃️) or a network call (🌐)
        liftEffect <<< log $ if inCache then "🗃️ " else "🌐 " <> resource
        -- count the effects
        if inCache then pure 0 else pure 1
