-- | module for all unit tests
module Test.Necklace.Unit
    -- exporting only the testSuite to get dead code warnings if anything else isn't used
    (tests) 
    where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Necklace (Necklace, focus, insertLeft, insertRight, next, prev, removeLeft, removeRight, size)
import Data.Necklace as Necklace
import Data.NonEmpty ((:|))
import Test.Utils (PN(..), walkNecklace)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.QuickCheck (quickCheck)


tests :: TestSuite
tests = suite "Necklace unit tests" do

    test "singleton" do
        let n = Necklace.singleton 0
        Assert.equal 0 (focus n)
        Assert.equal 0 (focus $ next n)
        Assert.equal 0 (focus $ prev n)
        Assert.equal 1 (size n)
        Assert.equal (0 :| []) (Necklace.toUnfoldable1 n)
    
    test "insertRight on singleton" do
        let n0 = Necklace.singleton 0
        let n1 = insertRight 1 n0
        Assert.equal 0 (focus n1)
        let n2 = next n1
        Assert.equal 1 (focus n2)
        let n3 = next n2
        Assert.equal 0 (focus n3)
        Assert.equal 2 (size n3)

    test "insertLeft on singleton" do
        let n0 = Necklace.singleton 0
        let n1 = insertLeft 1 n0
        Assert.equal 0 (focus n1)
        let n2 = prev n1
        Assert.equal 1 (focus n2)
        let n3 = prev n2
        Assert.equal 0 (focus n3)
        Assert.equal 2 (size n3)

    test "insertRight in the middle" do
        let xs = (0 :| [1, 3])
        let n0 = Necklace.fromNonEmpty xs
        Assert.equal 0 (focus n0)
        let n1 = next n0
        Assert.equal 1 (focus n1)
        Assert.equal 3 (size n1)
        let n2 = insertRight 2 n1
        Assert.equal 1 (focus n2)
        let n3 = next n2
        Assert.equal 2 (focus n3)
        let n4 = next n3
        Assert.equal 3 (focus n4)
        let n5 = next n4
        Assert.equal 0 (focus n5)
        Assert.equal 4 (size n5)

    test "fromNonEmpty" do
        let xs = (0 :| [1, 2])
        let n0 = Necklace.fromNonEmpty xs
        Assert.equal 0 (focus n0)
        let n1 = next n0
        Assert.equal 1 (focus n1)
        let n2 = next n1
        Assert.equal 2 (focus n2)
        let n3 = next n2
        Assert.equal 0 (focus n3)
        Assert.equal 3 (size n3)

    test "toUnfoldable1 on sigleton" do
        let necklace = Necklace.singleton 0
        let xs' = Necklace.toUnfoldable1 necklace
        let xs = [0]
        Assert.assert
            (show xs <> " != " <> show xs')
            (xs == xs')

    test "toUnfoldable1 on manually constructed necklace" do
        let n0 = Necklace.singleton 0
        let n1 = next $ insertRight 1 n0
        let n2 = next $ insertRight 2 n1
        let n3 = next $ next $ insertRight 3 n2
        let xs' = Necklace.toUnfoldable1 n3
        let xs = [0, 1, 2, 3]
        Assert.assert
            (show xs <> " != " <> show xs')
            (xs == xs')

    test "to and from NonEmptyArray" do
        let xs = (0 :| [1, 2, 3, 4])
        let necklace = Necklace.fromNonEmpty xs
        let xs' = Necklace.toUnfoldable1 necklace
        Assert.assert
            (show xs <> " != " <> show xs')
            (xs == xs')

    test "prev and next work as expected" $
        quickCheck (\(necklace :: Necklace Int) -> do
            let ys = Necklace.toUnfoldable1 necklace
            let n = focus (next necklace)
            let p = focus (prev necklace)
            case ys of
                [x] -> x == n && x == p
                xs  -> (Array.last xs) == (Just p)
                    && (Array.head =<< Array.tail xs) == (Just n)
        )

    test "walk twice around" do
        let necklace = Necklace.fromNonEmpty (0 :| [1, 2])
        Assert.equal [0, 1, 2, 0, 1, 2] (walkNecklace [N, N, N, N, N] necklace)
        Assert.equal [0, 2, 1, 0, 2, 1] (walkNecklace [P, P, P, P, P] necklace)

    test "removeRight" do
        Assert.equal Nothing (removeRight $ Necklace.singleton 0)
        let n0 = Necklace.fromNonEmpty (0 :| [1, 2])
        Assert.equal (Just [0, 2]) (Necklace.toUnfoldable1 <$> removeRight n0)

    test "removeLeft" do
        Assert.equal Nothing (removeLeft $ Necklace.singleton 0)
        let n0 = Necklace.fromNonEmpty (0 :| [1, 2])
        Assert.equal (Just [0, 1]) (Necklace.toUnfoldable1 <$> removeLeft n0)
