module Test.Laws.MCache where

import Prelude

import Data.MCache (MCache)
import Data.Maybe (Maybe)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)


laws :: TestSuite
laws = suite "mcache laws" do

    test "functor identity" $
        quickCheck (\(cache :: MCache Maybe Boolean) ->
            map identity cache == cache
        )
    
    test "functor composition" $
        quickCheck (\(cache :: MCache Maybe Int) ->
            let f = (_ + 1)
                g = (_ * 2)
            in (map (f <<< g) cache) == (map f <<< map g $ cache)
        )
