module Test.ZipperM.Laws (tests) where

import Prelude

import Control.Comonad (extend, extract, (<<=))
import Data.Identity (Identity)
import Data.ZipperM (ZipperM, focus)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)


-- | tests are written on the newtype wrapper MCacheM so that some values are forced
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

    test "extend associativity" $
        quickCheck (\(zipper :: ZipperM Identity Int) ->
            let f = (_ + 1) <<< focus
                g = (_ * 2) <<< focus
            in (extend f <<< extend g) zipper == (extend (f <<< extend g)) zipper
        )

    test "comonad left identity" $
        quickCheck (\(zipper :: ZipperM Identity Boolean) ->
            (extract <<= zipper) == zipper
        )

    test "comonad right identity" $
        quickCheck (\(zipper :: ZipperM Identity Int) ->
            let f = (_ + 1) <<< focus
            in (extract (f <<= zipper)) == f zipper
        )
