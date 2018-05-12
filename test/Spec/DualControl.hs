{-# LANGUAGE UndecidableInstances  #-}

module Spec.DualControl (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)

import Control.Monad.Freer (Eff, Member, send, run, reinterpret, interpret)
import Control.Monad.Freer.Writer (Writer, tell, runWriter)
import Data.Function ((&))
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

    testCase "emit attempt event when granted" $ do
        let
            -- given
                principals = ["natalie", "christopher"]
                reason = "something happened"

            -- when
                actual = grant principals reason

            -- then
        logged [(Attempt (sort principals) reason)] (const True) actual,

    testCase "emit attempt event when not" $ do
        let
        -- given
            principals = ["json"]
            reason = "something happened"

        -- when
            actual = grant principals reason

        -- then
        logged [(Attempt (sort principals) reason)] (const True) actual,

    testCase "log principals and reason when granted" $ do
        let
        -- given
            principals = ["natalie", "christopher"]
            reason = "something happened"

        -- when
            actual = grant principals reason

        -- then
        logged [Grant (sort principals) reason] (const True) actual,

    testCase "log not-authorized authorization denied" $ do
        let
        -- given
            principals = ["natalie"]
            reason = "something happened"

        -- when
            actual = grant principals reason

        -- then
        logged [NotAuthorized  (sort principals) [] reason] (const True) actual,

    testCase "log missing-reason when missing reason" $ do
        let
        -- given
            principals = ["charlene", "toby", "carlton" ]
            reason = ""

        -- when
            actual = grant principals reason

        -- then
        logged [MissingReason (sort principals)] (const True) actual,

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


data DualControlEvent where
    Attempt :: [Text] -> Text -> DualControlEvent
    Grant :: [Text] -> Text -> DualControlEvent
    NotAuthorized :: [Text] -> [Text] -> Text -> DualControlEvent
    MissingReason :: [Text] -> DualControlEvent
    deriving (Eq, Show)

-- type Log = [(Text, [Text], Bool)]

data DualControlEventStream a where
    Emit :: DualControlEvent -> DualControlEventStream ()

data Authorization a where
    Verify :: Text -> Authorization Bool

data Crypto a where
    Salt :: Crypto Text

class DualControl r where
    grant :: [Text] -> Text -> r (Maybe Text)

instance (Member Crypto effects, Member DualControlEventStream effects, Member Authorization effects) => DualControl (Eff effects) where
    grant principals reason = do
        send (Emit (Attempt (sort principals) reason))
        let unique = uniq principals
        verify <- mapM (send . Verify) unique
        if length (filter id verify) >= 2
            then if (T.null reason)
                    then do
                        send (Emit (MissingReason (sort principals)))
                        pure Nothing
                    else do
                        send (Emit (Grant (sort principals) reason))
                        salt <- send Salt
                        pure (Just salt)
            else do
                send (Emit (NotAuthorized (sort principals) [] reason))
                pure Nothing


runOp :: Text -> (Text -> Bool) -> Eff '[Crypto, DualControlEventStream, Authorization] (Maybe Text) -> (Maybe Text, [DualControlEvent])
runOp salt authz es =
    handleCrypto salt es & handleLog & handleAuth authz & run

handleLog :: Eff (DualControlEventStream ': effects) a -> Eff effects (a, [DualControlEvent])
handleLog es = runWriter $ reinterpret impl es
    where
        impl :: DualControlEventStream b -> Eff (Writer [DualControlEvent] ': effs) b
        impl (Emit l) = tell [l]

handleAuth :: (Text -> Bool) -> Eff (Authorization ': effects) a -> Eff effects a
handleAuth authz = interpret $ \(Verify prin) -> pure (authz prin)

handleCrypto :: Text -> Eff (Crypto ': effects) a -> Eff effects a
handleCrypto salt = interpret $ \Salt -> pure salt

response :: Text -> (Text -> Bool) -> Eff '[Crypto, DualControlEventStream, Authorization] (Maybe Text) -> Maybe Text
response salt authz es = fst $ runOp salt authz es

logged :: [DualControlEvent] -> (Text -> Bool) -> Eff '[Crypto, DualControlEventStream, Authorization] (Maybe Text) -> Assertion
logged expected authz es =
    let events = snd $ runOp "whatever" authz es
        actual = filter (\e -> elem e expected) events
    in actual === expected

uniq :: (Ord a) => [a] -> [a]
uniq [] = []
uniq as = head <$> (group . sort) as
