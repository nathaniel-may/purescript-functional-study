-- | module for all unit tests
module Test.BufferedZipper.Unit
    -- exporting only the testSuite to get dead code warnings if anything else isn't used
    (tests) 
    where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.State (State, evalState, get, modify)
import Data.BufferedZipper (focus, mkBufferedZipper, nextT, prevT)
import Data.BufferedZipper as BufferedZipper
import Data.Functor (voidLeft)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Assert as Assert
import Test.Utils (PN(..), walkBufferedZipper)
import Utils (runIdentity)


-- TODO add test for expected effects with the state monad
tests :: TestSuite
tests = suite "BufferedZipper unit tests" do

    test "next and prev" do
        let input = (pure <$> [0, 1] :: Array (Identity Int))
        let massert = runIdentity <<< runMaybeT $ do
                z0 <- MaybeT $ mkBufferedZipper 2 input
                z1 <- nextT z0
                z2 <- prevT z1
                _ <- pure $ assert "constructor failed" (focus z0 == 0)
                _ <- pure $ assert "next failed" (focus z1 == 1)
                pure $ assert "prev failed" (focus z2 == 0)
        case massert of
            Nothing -> assert "other failure" false
            Just assertion -> assertion

    test "next and prev foward and back buffer of 1" do
        let input = (pure <$> [0, 1, 2, 3, 4] :: Array (Identity Int))
        case runIdentity $ mkBufferedZipper 1 input of
            Nothing -> assert "failed to create the BufferedZipper" false
            Just zipper ->
                let values = runIdentity $ walkBufferedZipper [N, N, N, N, P, P, P, P] zipper
                in Assert.equal [0, 1, 2, 3, 4, 3, 2, 1, 0] values

    test "next and prev foward and back buffer of 2" do
        let input = (pure <$> [0, 1, 2, 3] :: Array (Identity Int))
        case runIdentity $ mkBufferedZipper 2 input of
            Nothing -> assert "failed to create the BufferedZipper" false
            Just zipper ->
                let values = runIdentity $ walkBufferedZipper [N, N, N, P, P, P] zipper
                in Assert.equal [0, 1, 2, 3, 2, 1, 0] values

    test "next and prev foward and back buffer of 3" do
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
    
    test "effect counts half buffer" do
        let count = modify (_ + 1)
        let input = (voidLeft count <$> ['a', 'b', 'c', 'd'] :: Array (State Int Char))
        let counter = do
                mz <- mkBufferedZipper 2 input
                _ <- case mz of
                    Nothing -> pure []
                    Just zipper -> walkBufferedZipper [N, N, N, P, P, P] zipper
                effectCount <- get
                pure $ Assert.equal 6 effectCount
        evalState counter 0

    test "effect counts all buffer" do
        let count = modify (_ + 1)
        let input = (voidLeft count <$> ['a', 'b', 'c', 'd'] :: Array (State Int Char))
        let counter = do
                mz <- mkBufferedZipper 4 input
                _ <- case mz of
                    Nothing -> pure []
                    Just zipper -> walkBufferedZipper [N, N, N, P, P, P] zipper
                effectCount <- get
                pure $ Assert.equal 4 effectCount
        evalState counter 0
