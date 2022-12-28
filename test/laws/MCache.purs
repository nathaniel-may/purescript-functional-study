module Test.Laws.MCache where

import Prelude

import Data.Identity (Identity)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)
import ZipperM.Test.Utils (MCacheM(..))
import ZipperM.Utils (runIdentity)


-- | tests are written on the newtype wrapper MCacheM so that some values are forced
laws :: TestSuite
laws = suite "mcache laws" do

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

    -- Composition: pure (<<<) <*> f <*> g <*> h = f <*> (g <*> h)
    -- Homomorphism: (pure f) <*> (pure x) = pure (f x)
    -- Interchange: u <*> (pure y) = (pure (_ $ y)) <*> u
