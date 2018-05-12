module Spec.DualControl (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)

import Data.List (sort, group)
import Data.Text (Text)
import qualified Data.Text as T (length)

(===) :: (Eq a, Show a) => a -> a -> Assertion
(===) = (@?=)
infix 1 ===

tests :: TestTree
tests = testGroup "DualControl" [
    testCase "grant a token to two principals" $ do
        let
        -- given
            token = "accessT"
            principal1 = "ace"
            principal2 = "tanya"
            principals = [principal1, principal2]

        -- when
            actual = grant principals "say so"

        -- then
        run token actual === Just token,

    testCase "grant a token to more than two principals" $ do
        let
        -- given
            token = "accessT"
            principal1 = "charlie"
            principal2 = "amy"
            principal3 = "mostro"
            principals = [principal1, principal2, principal3]

        -- when
            actual = grant principals "crash"

        -- then
        run token actual === Just token,

    testCase "do not grant a token to 1 principal" $ do
        let
        -- given
            token = "access tok"
            principal = "terry"
            principals = [principal]

        -- when
            actual = grant principals "overload"

        -- then
        run token actual === Nothing,

    testCase "grant when reason provided" $ do
        let
        -- given
            token = "acc"
            reason = "on fire"

        -- when
            actual = grant ["percival", "nona"] reason

        -- then
        run token actual === Just token,

    testCase "deny grant when reason is empty" $ do
        let
        -- given
            token = "cccca"
            reason = ""

        -- when
            actual = grant ["sheila", "thomas"]  reason

        -- then
        run token actual === Nothing,

    testCase "deny if fewer than 2 unique principals" $ do
        let
        -- given
            token = "accessT"
            principal1 = "charlie"
            principals = [principal1, principal1, principal1]

        -- when
            actual = grant principals "crash"

        -- then
        run token actual === Nothing


    ]


run :: Text -> (Text -> Maybe Text) -> Maybe Text
run = flip ($)

grant :: [Text] -> Text -> Text -> Maybe Text
grant principals reason | T.length reason > 0 && length (uniq principals) >= 2 = Just
                        | otherwise = const Nothing


uniq :: (Ord a) => [a] -> [a]
uniq [] = []
uniq as = head <$> (group . sort) as
