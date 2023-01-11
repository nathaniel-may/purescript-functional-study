module Test.RollingCacheT.Unit (tests) where

import Prelude

import Control.Monad.RollingCache.Class (class MonadRollingCache)
import Control.Monad.RollingCache.Class as Cache
import Control.Monad.RollingCacheT (RollingCacheT, evalRollingCacheT)
import Control.Monad.State (class MonadState, StateT(..), evalStateT, lift)
import Control.Monad.State as State
import Data.Maybe (Maybe(..))
import Data.RollingCache (RollingCache, mkRollingCache)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert


newtype TestM a = TestM (RollingCacheT Int Int (StateT Int Aff) a)

derive newtype instance functorTestM :: Functor TestM
derive newtype instance applyTestM :: Apply TestM
derive newtype instance applicativeTestM :: Applicative TestM
derive newtype instance bindTestM :: Bind TestM
derive newtype instance monadTestM :: Monad TestM
derive newtype instance monadEffectTestM :: MonadEffect TestM
derive newtype instance monadAffTestM :: MonadAff TestM
derive newtype instance monadRollingCacheTestM :: MonadRollingCache Int Int TestM

instance monadStateTestM :: MonadState Int TestM where
    state f = TestM <<< lift $ StateT (pure <<< f)

runTestM :: forall a. TestM a -> RollingCache Int Int -> Int -> Aff a
runTestM (TestM m) c = evalStateT (evalRollingCacheT m c)

tests :: TestSuite
tests = suite "RollingCacheT unit tests" do

    test "insert lookup loop" $ do
        let theTest = do
                Cache.insert 1 100
                v <- Cache.retrieve 1
                liftAff $ Assert.equal (Just 100) v
        runTestM theTest (mkRollingCache $ Nothing) 0

    test "fetch" $ do
        let theTest = do
                let effectfulFetch x = (State.modify_ (_ + 1) $> (x * 100) :: TestM Int)
                -- fetch runs an effect because the cache is empty
                _ <- Cache.fetch effectfulFetch 1
                effects0 <- State.get
                liftAff $ Assert.equal 1 effects0
                -- fetch does not run an effect because the key is in the cache
                _ <- Cache.fetch effectfulFetch 1
                effects1 <- State.get
                liftAff $ Assert.equal 1 effects1
        runTestM theTest (mkRollingCache $ Nothing) 0

    test "effect counts" $ do
        let theTest expectedEffects = do
                let input = [1, 2, 3, 4, 3, 4, 3, 1]
                let effectfulFetch x = (State.modify_ (_ + 1) $> (x * 100) :: TestM Int)
                _ <- traverse (Cache.fetch effectfulFetch) input :: TestM (Array Int)
                effects <- (State.get :: TestM Int)
                liftAff $ Assert.equal expectedEffects effects
        runTestM (theTest 4) (mkRollingCache $ Nothing) 0
        runTestM (theTest 8) (mkRollingCache $ Just 1) 0
        runTestM (theTest 5) (mkRollingCache $ Just 2) 0
