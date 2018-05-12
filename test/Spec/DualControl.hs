module Spec.DualControl (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)

import Data.List (sort, group)
import Data.Text (Text)
import qualified Data.Text as T (null)

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
        response token (const True) actual === Just token,

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
        response token (const True) actual === Just token,

    testCase "do not grant a token to 1 principal" $ do
        let
        -- given
            token = "access tok"
            principal = "terry"
            principals = [principal]

        -- when
            actual = grant principals "overload"

        -- then
        response token (const True) actual === Nothing,

    testCase "grant when reason provided" $ do
        let
        -- given
            token = "acc"
            reason = "on fire"
            principals = ["percival", "nona"]

        -- when
            actual = grant principals reason

        -- then
        response token (const True) actual === Just token,

    testCase "deny grant when reason is empty" $ do
        let
        -- given
            token = "cccca"
            reason = ""
            principals = ["sheila", "thomas"]

        -- when
            actual = grant principals reason

        -- then
        response token (const True) actual === Nothing,

    testCase "deny if fewer than 2 unique principals" $ do
        let
        -- given
            token = "accessT"
            principal1 = "charlie"
            principals = [principal1, principal1, principal1]

        -- when
            actual = grant principals "crash"

        -- then
        response token  (const True) actual === Nothing,

    testCase "log principals and reason when granted" $ do
        let
        -- given
            token = "Tac"
            principals = ["natalie", "christopher"]
            reason = "something happened"

        -- when
            actual = grant principals reason

        -- then
        logged token (const True) actual === [(reason, sort principals, True)],

    testCase "log principals and reason when denied" $ do
        let
        -- given
            token = "Tac"
            principals = ["natalie"]
            reason = "something happened"

        -- when
            actual = grant principals reason

        -- then
        logged token (const True) actual === [(reason, sort principals, False)],

    testCase "denies unauthorized principals" $ do
        let
        -- given
            token = "you'll never see this"
            authorized = "josie"
            unauthorized = "tom"
            principals = [authorized, unauthorized]

        -- when
            actual = grant principals "rain"

        -- then
        response token (== authorized) actual === Nothing

    ]

type Log = [(Text, [Text], Bool)]

response :: Text -> (Text -> Bool) -> ((Text -> Bool) -> Text -> (Maybe Text, Log)) -> Maybe Text
response tok authd f = fst $ f authd tok

logged :: Text -> (Text -> Bool) -> ((Text -> Bool) -> Text -> (Maybe Text, Log)) -> [(Text, [Text], Bool)]
logged tok authd f = snd $ f authd tok

grant :: [Text] -> Text -> (Text -> Bool) -> Text -> (Maybe Text, Log)
grant principals reason authd | not (T.null reason) && authorize principals authd = \tok -> (Just tok, [(reason, sort principals, True)])
                                   | otherwise = const (Nothing, [(reason, sort principals, False)])


authorize :: [Text] -> (Text -> Bool) -> Bool
authorize principals authz = length (filter authz (uniq principals)) >= 2

uniq :: (Ord a) => [a] -> [a]
uniq [] = []
uniq as = head <$> (group . sort) as
