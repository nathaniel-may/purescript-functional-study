module Test.FlexStateT.Laws (tests) where

import Prelude

import Control.FlexStateT (FlexStateT, runFlexStateT)
import Data.Identity (Identity)
import Data.Tuple (Tuple)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)


-- | tests are written on the newtype wrapper MCacheM so that some values are forced
tests :: TestSuite
tests = suite "FlexStateT laws" do

    test "functor identity" $
        quickCheck (\(m :: FlexStateT (Array Int) (Array Boolean) Identity Boolean) (s :: Array Int) ->
            runFlexStateT (map identity m) s == (runFlexStateT m s)
        )
    
    test "functor composition" $
        quickCheck (\(m :: FlexStateT (Array Int) (Array Boolean) Identity Int) (s :: Array Int) ->
            let f = (_ + 1)
                g = (_ * 2)
            in runFlexStateT (map (f <<< g) m) s == runFlexStateT (map f <<< map g $ m) s
        )

    test "applicative identity" $
        quickCheck (\(m :: FlexStateT (Array Int) (Array Boolean) Identity Boolean) (s :: Array Int) ->
            runFlexStateT (pure identity <*> m) s == runFlexStateT m s
        )

    test "applicative composition" $
        quickCheck (\(m :: FlexStateT (Array Int) (Array Boolean) Identity Int) (s :: Array Int) ->
            let f = pure (_ + 1)
                g = pure (_ * 2)
            in runFlexStateT (pure (<<<) <*> f <*> g <*> m) s == runFlexStateT (f <*> (g <*> m)) s
        )

    test "applicative associative composition" $
        quickCheck (\(m :: FlexStateT (Array Int) (Array Boolean) Identity Int) (s :: Array Int) ->
            let f = pure (_ + 1)
                g = pure (_ * 2)
            in runFlexStateT ((<<<) <$> f <*> g <*> m) s == runFlexStateT (f <*> (g <*> m)) s
        )

    test "applicative homomorphism" $
        quickCheck (\(x :: Int) (s :: Array Int) ->
            let f = (_ + 1)
            in runFlexStateT ((pure f) <*> (pure x)) s == (runFlexStateT (pure (f x)) s :: Identity (Tuple Int (Array Boolean)))
        )

    test "applicative interchange" $
        quickCheck (\(x :: Int) (s :: Array Int) ->
            let f = pure (_ - 1) :: FlexStateT (Array Int) (Array Boolean) Identity (Int -> Int)
            in runFlexStateT (f <*> (pure x)) s == runFlexStateT ((pure (_ $ x)) <*> f) s
        )

    -- TODO add these tests:
    -- Bind Associativity: (x >>= f) >>= g = x >>= (\k -> f k >>= g)
    -- Bind Apply Superclass: apply f x = f >>= \f’ -> map f’ x
    -- Monad Left Identity: pure x >>= f = f x
    -- Monad Right Identity: x >>= pure = x
