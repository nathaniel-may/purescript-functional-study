module Test.Zipper.Laws (tests) where

import Prelude

import Control.Comonad (extract)
import Control.Extend (extend)
import Data.Zipper (Zipper, focus)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)


tests :: TestSuite
tests = suite "Zipper laws" do

    test "functor identity" $
        quickCheck (\(zipper :: Zipper Boolean) ->
            map identity zipper == zipper
        )
    
    test "functor composition" $
        quickCheck (\(zipper :: Zipper Int) ->
            let f = (_ + 1)
                g = (_ * 2)
            in (map (f <<< g) zipper) == (map f <<< map g $ zipper)
        )

    test "extend associativity" $
        quickCheck (\(zipper :: Zipper Int) ->
            let f = \x -> (focus x) + 1
                g = \y -> (focus y) * 2
            in (extend f <<< extend g) zipper == (extend (f <<< extend g)) zipper
        )

    test "comonad left identity" $
        quickCheck (\(zipper :: Zipper Int) ->
            extend extract zipper == zipper
        )

    test "comonad right identity" $
        quickCheck (\(zipper :: Zipper Int) ->
            let f = \x -> (focus x) + 1
            in extract (extend f zipper) == f zipper
        )
