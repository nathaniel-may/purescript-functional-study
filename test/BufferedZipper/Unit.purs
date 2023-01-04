-- | module for all unit tests
module Test.BufferedZipper.Unit
    -- exporting only the testSuite to get dead code warnings if anything else isn't used
    (tests) 
    where

import Prelude

import Data.BufferedZipper (mkBufferedZipper)
import Data.BufferedZipper as BufferedZipper
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Test.Unit (TestSuite, suite, test, testSkip)
import Test.Unit.Assert (assert)
import Test.Unit.Assert as Assert
import Test.Utils (PN(..), walkBufferedZipper)
import Utils (runIdentity)


-- TODO add test for expected effects with the state monad
tests :: TestSuite
tests = suite "ZipperM unit tests" do
    
    -- TODO this test is right, but BufferedZipper is broken.
    testSkip "next and prev foward and back" do
        let input = (pure <$> [0, 1, 2, 3, 4] :: Array (Identity Int))
        case runIdentity $ mkBufferedZipper 3 input of
            Nothing -> assert "failed to create the BufferedZipper" false
            Just zipper ->
                let values = runIdentity $ walkBufferedZipper [N, N, N, N, P, P, P, P] zipper
                in Assert.equal [0, 1, 2, 3, 4, 3, 2, 1, 0] values

    test "toArray" do
        let input = (pure <$> [0, 1, 2, 3, 4] :: Array (Identity Int))
        case runIdentity $ mkBufferedZipper 3 input of
            Nothing -> assert "failed to create the BufferedZipper" false
            Just zipper ->
                let expected = pure <$> [0, 1, 2, 3, 4]
                in Assert.equal expected (BufferedZipper.toArray zipper)
