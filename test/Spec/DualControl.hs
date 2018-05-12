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

    testCase "log principals and reason when granted" $ do
        let
        -- given
            token = "Tac"
            principals = ["natalie", "christopher"]
            reason = "something happened"

        -- when
            actual = grant principals reason

        -- then
        logged token (const True) actual === [Grant (sort principals) reason],

    testCase "log not-authorized authorization denied" $ do
        let
        -- given
            token = "Tac"
            principals = ["natalie"]
            reason = "something happened"

        -- when
            actual = grant principals reason

        -- then
        logged token (const True) actual === [NotAuthorized  (sort principals) [] reason],

    testCase "log missing-reason when missing reason" $ do
        let
        -- given
            token = "Tac"
            principals = ["charlene", "toby", "carlton" ]
            reason = ""

        -- when
            actual = grant principals reason

        -- then
        logged token (const True) actual === [MissingReason (sort principals)],

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


data Log where
    Grant :: [Text] -> Text -> Log
    NotAuthorized :: [Text] -> [Text] -> Text -> Log
    MissingReason :: [Text] -> Log
    deriving (Eq, Show)

-- type Log = [(Text, [Text], Bool)]

data AccessLog a where
    Emit :: Log -> AccessLog ()

data Authorization a where
    Verify :: Text -> Authorization Bool

data Crypto a where
    Salt :: Crypto Text

class DualControl r where
    grant :: [Text] -> Text -> r (Maybe Text)

instance (Member Crypto effects, Member AccessLog effects, Member Authorization effects) => DualControl (Eff effects) where
    grant principals reason = do

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


runOp :: Text -> (Text -> Bool) -> Eff '[Crypto, AccessLog, Authorization] (Maybe Text) -> (Maybe Text, [Log])
runOp salt authz es =
    handleCrypto salt es & handleLog & handleAuth authz & run

handleLog :: Eff (AccessLog ': effects) a -> Eff effects (a, [Log])
handleLog es = runWriter $ reinterpret impl es
    where
        impl :: AccessLog b -> Eff (Writer [Log] ': effs) b
        impl (Emit l) = tell [l]

handleAuth :: (Text -> Bool) -> Eff (Authorization ': effects) a -> Eff effects a
handleAuth authz = interpret $ \(Verify prin) -> pure (authz prin)

handleCrypto :: Text -> Eff (Crypto ': effects) a -> Eff effects a
handleCrypto salt = interpret $ \Salt -> pure salt

response :: Text -> (Text -> Bool) -> Eff '[Crypto, AccessLog, Authorization] (Maybe Text) -> Maybe Text
response salt authz es = fst $ runOp salt authz es

logged :: Text -> (Text -> Bool) -> Eff '[Crypto, AccessLog, Authorization] (Maybe Text) -> [Log]
logged salt authz es = snd $ runOp salt authz es

uniq :: (Ord a) => [a] -> [a]
uniq [] = []
uniq as = head <$> (group . sort) as
