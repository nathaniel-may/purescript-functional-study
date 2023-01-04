module Test.Necklace.Laws (tests) where

import Prelude

import Control.Comonad (extend, extract)
import Data.Necklace (Necklace, focus)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)


tests :: TestSuite
tests = suite "Necklace laws" do

    test "functor identity" $
        quickCheck (\(necklace :: Necklace Int) ->
            map identity necklace == necklace
        )
    
    test "functor composition" $
        quickCheck (\(necklace :: Necklace Int) ->
            let f = (_ + 1)
                g = (_ * 2)
            in (map (f <<< g) necklace) == (map f <<< map g $ necklace)
        )

    test "extend associativity" $
        quickCheck (\(necklace :: Necklace Int) ->
            let f = \x -> (focus x) + 1
                g = \y -> (focus y) * 2
            in (extend f <<< extend g) necklace == (extend (f <<< extend g)) necklace
        )

    test "comonad left identity" $
        quickCheck (\(necklace :: Necklace Int) ->
            extend extract necklace == necklace
        )

    test "comonad right identity" $
        quickCheck (\(necklace :: Necklace Int) ->
            let f = \x -> (focus x) + 1
            in extract (extend f necklace) == f necklace
        )
