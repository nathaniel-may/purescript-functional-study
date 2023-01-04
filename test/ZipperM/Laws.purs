module Test.ZipperM.Laws (tests) where

import Prelude

import Data.Identity (Identity)
import Data.ZipperM (ZipperM)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)


tests :: TestSuite
tests = suite "ZipperM laws" do

    test "functor identity" $
        quickCheck (\(zipper :: ZipperM Identity Boolean) ->
            map identity zipper == zipper
        )
    
    test "functor composition" $
        quickCheck (\(zipper :: ZipperM Identity Int) ->
            let f = (_ + 1)
                g = (_ * 2)
            in (map (f <<< g) zipper) == (map f <<< map g $ zipper)
        )
