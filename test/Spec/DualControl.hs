{-# LANGUAGE UndecidableInstances  #-}

module Spec.DualControl (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)

import Control.Monad.Freer (Eff, Member, send, run, reinterpret, interpret)
import Control.Monad.Freer.Writer (Writer, tell, runWriter)
import Data.Function ((&))
import Data.HashSet (fromList, HashSet)
import Data.List (sort, group)
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import qualified Data.Text as T (null)

(===) :: (Eq a, Show a) => a -> a -> Assertion
(===) = (@?=)
infix 1 ===

tests :: TestTree
tests = testGroup "DualControl" [

    testCase "single principal token" $ do
        let
        -- given
            principal = "Sonny"
            reason = "I say so"
            now = UTCTime (toEnum 1903092) 12

        -- when
            actual = grant' [principal] reason

        -- then
        grantedTo actual (const True) === fromList [principal],
        -- grantedUntil actual (const True) now === addUTCTime (realToFrac 3600) now,

    testCase "grant a token to two principals" $ do
        let
        -- given
            principal1 = "ace"
            principal2 = "tanya"
            principals = [principal1, principal2]

        -- when
            actual = grant principals "say so"

        -- then
        granted (const True) actual,

    testCase "grant a token to more than two principals" $ do
        let
        -- given
            principal1 = "charlie"
            principal2 = "amy"
            principal3 = "mostro"
            principals = [principal1, principal2, principal3]

        -- when
            actual = grant principals "crash"

        -- then
        granted (const True) actual,

    testCase "do not grant a token to 1 principal" $ do
        let
        -- given
            principal = "terry"
            principals = [principal]

        -- when
            actual = grant principals "overload"

        -- then
        denied (const True) actual,

    testCase "grant when reason provided" $ do
        let
        -- given
            reason = "on fire"
            principals = ["percival", "nona"]

        -- when
            actual = grant principals reason

        -- then
        granted (const True) actual,

    testCase "deny grant when reason is empty" $ do
        let
        -- given
            reason = ""
            principals = ["sheila", "thomas"]

        -- when
            actual = grant principals reason

        -- then
        denied (const True) actual,

    testCase "deny if fewer than 2 unique principals" $ do
        let
        -- given
            principal1 = "charlie"
            principals = [principal1, principal1, principal1]

        -- when
            actual = grant principals "crash"

        -- then
        denied (const True) actual,

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
            authorized = "josie"
            unauthorized = "tom"
            principals = [authorized, unauthorized]

        -- when
            actual = grant principals "rain"

        -- then
        denied (== authorized) actual

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
    grant' :: [Text] -> Text -> r (Maybe Token)


data Token = Token {
    principals :: HashSet Text,
    reason :: Text
}


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
    grant' principals reason = pure $ Just $ Token (fromList principals) reason


runOp :: Text -> (Text -> Bool) -> Eff '[Crypto, DualControlEventStream, Authorization] a -> (a, [DualControlEvent])
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

denied :: (Text -> Bool) -> Eff '[Crypto, DualControlEventStream, Authorization] (Maybe Text) -> Assertion
denied authz es = (fst $ runOp "whatever" authz es) === Nothing

granted :: (Text -> Bool) -> Eff '[Crypto, DualControlEventStream, Authorization] (Maybe Text) -> Assertion
granted authz es =
    let salt = "salty"
        response = fst $ runOp salt authz es
    in response === Just salt

type Principal = Text

grantedTo :: Eff '[Crypto, DualControlEventStream, Authorization] (Maybe Token) -> (Principal -> Bool) -> HashSet Text
grantedTo es authz = maybe (fromList []) principals (fst (runOp "salt" authz es))

-- grantedUntil :: Eff '[Crypto, DualControlEventStream, Authorization] (Maybe Token) -> (Principal -> Bool) -> UTCTime -> UTCTime
-- grantedUntil = error "NYI"

logged :: [DualControlEvent] -> (Text -> Bool) -> Eff '[Crypto, DualControlEventStream, Authorization] (Maybe Text) -> Assertion
logged expected authz es =
    let events = snd $ runOp "whatever" authz es
        actual = filter (\e -> elem e expected) events
    in actual === expected

uniq :: (Ord a) => [a] -> [a]
uniq [] = []
uniq as = head <$> (group . sort) as
