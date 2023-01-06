-- | module for all unit tests
module Test.ZipperM.Unit
    -- exporting only the testSuite to get dead code warnings if anything else isn't used
    (tests) 
    where

import Prelude

import Data.Identity (Identity)
import Data.List.Lazy (List, fromFoldable, nil)
import Data.List.Lazy as List
import Data.ZipperM (ZipperM(..))
import Data.ZipperM as ZipperM
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Utils (PN(..), walkZipperM)
import Utils (runIdentity)


tests :: TestSuite
tests = suite "ZipperM unit tests" do

    test "next and prev foward and back" do
        let input = (fromFoldable $ pure <$> [1, 2]) :: List (Identity Int)
        let zipper = ZipperM.fromList1 0 input
        let values = runIdentity $ walkZipperM [N, N, P, P, P] zipper
        Assert.equal [0, 1, 2, 1, 0] values

    test "toList" do
        let input = (fromFoldable $ pure <$> [1, 2, 3, 4]) :: List (Identity Int)
        let zipper = ZipperM.fromList1 0 input
        let expected = List.fromFoldable $ pure <$> [0, 1, 2, 3, 4]
        Assert.equal expected (ZipperM.toList zipper)

    test "toArray" do
        let input = (fromFoldable $ pure <$> [1, 2, 3, 4]) :: List (Identity Int)
        let zipper = ZipperM.fromList1 0 input
        let expected = pure <$> [0, 1, 2, 3, 4]
        Assert.equal expected (ZipperM.toArray zipper)

    test "fromList1" do
        let input = (fromFoldable $ pure <$> [1, 2, 3, 4]) :: List (Identity Int)
        let zipper = ZipperM.fromList1 0 input
        let expected = ZipperM nil 0 (fromFoldable $ pure <$> [1, 2, 3, 4])
        Assert.assert "" (expected == zipper)
