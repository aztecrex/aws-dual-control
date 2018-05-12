module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Spec.DualControl as DualControl (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all" [
        DualControl.tests
    ]
