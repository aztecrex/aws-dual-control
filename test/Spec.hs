module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Spec.AccessControl as AccessControl (tests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all" [
        AccessControl.tests
    ]
