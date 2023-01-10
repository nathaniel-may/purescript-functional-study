module Test.RollingCache.Unit
    -- exporting only the testSuite to get dead code warnings if anything else isn't used
    (tests) 
    where

import Prelude

import Data.Array as Array
import Data.HashMap as M
import Data.Maybe (Maybe(..))
import Data.RollingCache (RollingCache(..), mkRollingCache)
import Data.RollingCache as Cache
import Data.Tuple (Tuple(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert


tests :: TestSuite
tests = suite "RollingCache unit tests" do

    test "cache rolls" do
        let cache0 = mkRollingCache (Just 2)
        let cache1 = Cache.insert 1 "a" cache0
        let (Tuple v0 cache2) = Cache.lookup 1 cache1
        Assert.equal v0 (Just "a")
        let cache3 = Cache.insert 2 "b" cache2
        let (Tuple v1 cache4) = Cache.lookup 1 cache3
        Assert.equal v1 (Just "a")
        let (Tuple v2 cache5) = Cache.lookup 2 cache4
        Assert.equal v2 (Just "b")
        let cache6 = Cache.insert 3 "c" cache5
        let (Tuple v1 cache7) = Cache.lookup 1 cache6
        Assert.equal v1 Nothing
        let (Tuple v2 cache8) = Cache.lookup 2 cache7
        Assert.equal v2 (Just "b")
        let (Tuple v2 _) = Cache.lookup 3 cache8
        Assert.equal v2 (Just "c")

    test "fromHashMap shrinks appropriately" do
        let (RollingCache max ks m) = Cache.fromHashMap (Just 2) $ M.fromArray
                [ Tuple 1 "a"
                , Tuple 2 "b"
                , Tuple 3 "c"
                , Tuple 4 "d"
                , Tuple 5 "e"
                ]
        Assert.equal (Just 2) max
        Assert.equal 2 (M.size m)
        Assert.equal 2 (Array.length ks)
        Assert.equal ks (M.keys m)

    test "pinning and unpinning values" do
        let cache0 = Cache.fromHashMap (Just 2) $ M.fromArray
                [ Tuple 1 "a"
                , Tuple 2 "b"
                ]
        case Cache.pin 1 "a" cache0 of
            Nothing -> Assert.assert "pin failed" false
            Just cache1 -> do
                let cache2 = Cache.insert 3 "c" cache1
                let cache3 = Cache.insert 4 "d" cache2
                let (Tuple v0 cache4) = Cache.lookup 1 cache3
                Assert.equal (Just "a") v0
                Assert.equal 2 (Cache.size cache4)
                let cache5 = Cache.unpin 1 cache4
                let (Tuple v1 cache5) = Cache.lookup 1 cache5
                Assert.equal (Just "a") v1
                Assert.equal 2 (Cache.size cache5)
                let cache6 = Cache.insert 5 "e" cache5
                let cache7 = Cache.insert 6 "f" cache6
                let (Tuple v2 _) = Cache.lookup 1 cache7
                Assert.equal Nothing v2
                Assert.equal 2 (Cache.size cache7)
