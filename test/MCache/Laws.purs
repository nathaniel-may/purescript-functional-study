module Test.MCache.Laws (tests) where

import Prelude

import Control.Comonad (extend)
import Data.Identity (Identity)
import Data.MCache (MCache(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)
import ZipperM.Test.Utils (MCacheM(..))
import ZipperM.Utils (runIdentity)


-- | tests are written on the newtype wrapper MCacheM so that some values are forced
tests :: TestSuite
tests = suite "MCache laws" do

    test "functor identity" $
        quickCheck (\(MCacheM mcache :: MCacheM Identity Boolean) ->
            let cache = runIdentity mcache
            in map identity cache == cache
        )
    
    test "functor composition" $
        quickCheck (\(MCacheM mcache :: MCacheM Identity Int) ->
            let cache = runIdentity mcache
                f = (_ + 1)
                g = (_ * 2)
            in (map (f <<< g) cache) == (map f <<< map g $ cache)
        )

    test "applicative identity" $
        quickCheck (\(MCacheM mcache :: MCacheM Identity Boolean) ->
            let cache = runIdentity mcache
            in (pure identity <*> cache) == cache
        )

    test "applicative composition" $
        quickCheck (\(MCacheM mcache :: MCacheM Identity Int) ->
            let cache = runIdentity mcache
                f = pure (_ + 1)
                g = pure (_ * 2)
            in (pure (<<<) <*> f <*> g <*> cache) == (f <*> (g <*> cache))
        )

    test "applicative associative composition" $
        quickCheck (\(MCacheM mcache :: MCacheM Identity Int) ->
            let cache = runIdentity mcache
                f = pure (_ + 1)
                g = pure (_ * 2)
            in ((<<<) <$> f <*> g <*> cache) == (f <*> (g <*> cache))
        )

    test "applicative homomorphism" $
        quickCheck (\(x :: Int) ->
            let f = (_ + 1)
            in ((pure f) <*> (pure x)) == (pure (f x) :: MCache Identity Int)
        )

    test "applicative interchange" $
        quickCheck (\(x :: Int) ->
            let f = pure (_ - 1) :: MCache Identity (Int -> Int)
            in (f <*> (pure x)) == ((pure (_ $ x)) <*> f)
        )

    test "extend associativity" $
        quickCheck (\(MCacheM mcache :: MCacheM Identity Int) ->
            let cache = runIdentity mcache
                f = \(MCache _ my) -> my
                g = \(MCache _ my) -> my
            in (extend f <<< extend g) cache == (extend (f <<< extend g)) cache
        )
