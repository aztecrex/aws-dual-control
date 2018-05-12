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
        response token actual === Just token,

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
        response token actual === Just token,

    testCase "do not grant a token to 1 principal" $ do
        let
        -- given
            token = "access tok"
            principal = "terry"
            principals = [principal]

        -- when
            actual = grant principals "overload"

        -- then
        response token actual === Nothing,

    testCase "grant when reason provided" $ do
        let
        -- given
            token = "acc"
            reason = "on fire"

        -- when
            actual = grant ["percival", "nona"] reason

        -- then
        response token actual === Just token,

    testCase "deny grant when reason is empty" $ do
        let
        -- given
            token = "cccca"
            reason = ""

        -- when
            actual = grant ["sheila", "thomas"]  reason

        -- then
        response token actual === Nothing,

    testCase "deny if fewer than 2 unique principals" $ do
        let
        -- given
            token = "accessT"
            principal1 = "charlie"
            principals = [principal1, principal1, principal1]

        -- when
            actual = grant principals "crash"

        -- then
        response token actual === Nothing,

    testCase "log principals and reason when granted" $ do
        let
        -- given
            token = "Tac"
            principals = ["natalie", "christopher"]
            reason = "something happened"

        -- when
            actual = grant principals reason

        -- then
        logged token actual === [(reason, sort principals, True)]


    ]

type Log = [(Text, [Text], Bool)]

response :: Text -> (Text -> (Maybe Text, Log)) -> Maybe Text
response tok f = fst $ f tok

logged :: Text -> (Text -> (Maybe Text, Log)) -> [(Text, [Text], Bool)]
logged tok f = snd $ f tok

grant :: [Text] -> Text -> Text -> (Maybe Text, Log)
grant principals reason | T.length reason > 0 && length (uniq principals) >= 2 = \tok -> (Just tok, [(reason, sort principals, True)])
                        | otherwise = const (Nothing, [])


uniq :: (Ord a) => [a] -> [a]
uniq [] = []
uniq as = head <$> (group . sort) as
