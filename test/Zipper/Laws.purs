module Test.Zipper.Laws (tests) where

import Prelude

import Data.Zipper (Zipper)
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
