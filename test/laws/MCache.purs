module Test.Laws.MCache where

import Prelude

import Data.Identity (Identity)
import Data.MCache (MCache, force)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)
import ZipperM.Utils (runIdentity)


laws :: TestSuite
laws = suite "mcache laws" do

    test "functor identity" $
        quickCheck (\(cache :: MCache Identity Boolean) ->
            map identity cache == cache
        )
    
    test "functor identity forced" $
        quickCheck (\(cache :: MCache Identity Boolean) -> runIdentity do
            cache' <- force cache
            pure $ map identity cache' == cache'
        )
    
    test "functor composition" $
        quickCheck (\(cache :: MCache Identity Int) ->
            let f = (_ + 1)
                g = (_ * 2)
            in (map (f <<< g) cache) == (map f <<< map g $ cache)
        )

    test "functor composition forced" $
        quickCheck (\(cache :: MCache Identity Int) -> runIdentity do
            cache' <- force cache
            let f = (_ + 1)
            let g = (_ * 2)
            pure $ (map (f <<< g) cache') == (map f <<< map g $ cache')
        )

    test "applicative identity" $
        quickCheck (\(cache :: MCache Identity Boolean) ->
            (pure identity <*> cache) == cache
        )

    test "applicative identity forced" $
        quickCheck (\(cache :: MCache Identity Boolean) -> runIdentity do
            cache' <- force cache
            pure $ (pure identity <*> cache') == cache'
        )

    -- Composition: pure (<<<) <*> f <*> g <*> h = f <*> (g <*> h)
    -- Homomorphism: (pure f) <*> (pure x) = pure (f x)
    -- Interchange: u <*> (pure y) = (pure (_ $ y)) <*> u
