module Spec.DualControl (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)

import Data.Text (Text)

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
            actual = vend principals

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
            actual = vend principals

        -- then
        run token actual === Just token,

    testCase "do not grant a token to 1 principal" $ do
        let
        -- given
            token = "access tok"
            principal = "terry"
            principals = [principal]

        -- when
            actual = vend principals

        -- then
        run token actual === Nothing

    ]


run :: Text -> (Text -> Maybe Text) -> Maybe Text
run = flip ($)

vend :: [Text] -> Text -> Maybe Text
vend principals | length principals >= 2 = Just
                | otherwise = const Nothing



