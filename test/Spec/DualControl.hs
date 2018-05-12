{-# LANGUAGE UndecidableInstances  #-}

module Spec.DualControl (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)

import Control.Monad.Freer (Eff, Members, send, run, reinterpret, interpret)
import Control.Monad.Freer.Writer (Writer, tell, runWriter)
import Data.Function ((&))
import Data.HashSet (fromList, HashSet)
import Data.List (sort, group)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import qualified Data.Text as T (null)

(===) :: (Eq a, Show a) => a -> a -> Assertion
(===) = (@?=)
infix 1 ===

tests :: TestTree
tests = testGroup "DualControl" [

    testCase "single principal token result" $ do
        let
        -- given
            principal = "Sonny"
            reason = "I say so"
            now = UTCTime (toEnum 40001) 12

        -- when
            actual = grant' principal reason

        -- then
        grantedTo actual (const True) === fromList [principal]
        grantedUntil actual (const True) now === addUTCTime (realToFrac (3600 :: Int)) now
        grantedFor actual (const True) === reason,

    testCase "single principal unauthorized result" $ do
        let
        -- given
            principal = "Sonny"
            reason = "I say so"

        -- when
            actual = grant' principal reason

        -- then
        notGranted actual (const False),

    testCase "single principal emit attempt and success event" $ do
        let
        -- when
            principal = "Cor"
            reason = "rain"
            actual = grant' principal reason

        -- then
        logged [
            (Attempt (fromList [principal]) reason),
            (Grant' (fromList [principal]) reason)
            ] (const True) actual,

    testCase "single principal unauthorized emit attempt and success event" $ do
        let
        -- when
            principal = "Cor"
            reason = "rain"
            actual = grant' principal reason

        -- then
        logged [
            (Attempt (fromList [principal]) reason),
            (Unauthorized (fromList [principal]) reason)
            ] (const False) actual,

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
        logged [(Attempt (fromList principals) reason)] (const True) actual,

    testCase "emit attempt event when not" $ do
        let
        -- given
            principals = ["json"]
            reason = "something happened"

        -- when
            actual = grant principals reason

        -- then
        logged [(Attempt (fromList principals) reason)] (const True) actual,

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
    Attempt :: HashSet Text -> Text -> DualControlEvent
    Grant :: [Text] -> Text -> DualControlEvent
    Grant' :: HashSet Text -> Text -> DualControlEvent
    Unauthorized :: HashSet Text -> Text -> DualControlEvent
    NotAuthorized :: [Text] -> [Text] -> Text -> DualControlEvent
    MissingReason :: [Text] -> DualControlEvent
    deriving (Eq, Show)

data DualControlEventStream a where
    Emit :: DualControlEvent -> DualControlEventStream ()

data Authorization a where
    Verify :: Text -> Authorization Bool

data Crypto a where
    Salt :: Crypto Text

data Clock a where
    Now :: Clock UTCTime

class DualControl r where
    grant :: [Text] -> Text -> r (Maybe Text)
    grant' :: Text -> Text -> r (Maybe Token)


data Token = Token {
    principals :: HashSet Text,
    reason :: Text,
    expiration :: UTCTime
}


instance (Members '[Clock, Crypto, DualControlEventStream, Authorization] effects) => DualControl (Eff effects) where
    grant principals reason = do
        send (Emit (Attempt (fromList principals) reason))
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
    grant' principal reason = do
        send (Emit (Attempt (fromList [principal]) reason))
        now <- send Now
        authorized <- send (Verify principal)
        if authorized
            then do
                let expiration = addUTCTime (realToFrac (3600 :: Int)) now
                send (Emit (Grant' (fromList [principal]) reason) )
                pure $ Just $ Token (fromList [principal]) reason expiration
            else do
                send (Emit (Unauthorized (fromList [principal]) reason ))
                pure Nothing


epoch :: UTCTime
epoch =(UTCTime (toEnum 0) 0)

runOp :: Text -> (Text -> Bool) -> UTCTime -> Eff '[Clock, Crypto, DualControlEventStream, Authorization] a -> (a, [DualControlEvent])
runOp salt authz now es =
    handleClock now es & handleCrypto salt & handleLog & handleAuth authz & run

handleLog :: Eff (DualControlEventStream ': effects) a -> Eff effects (a, [DualControlEvent])
handleLog es = runWriter $ reinterpret impl es
    where
        impl :: DualControlEventStream b -> Eff (Writer [DualControlEvent] ': effs) b
        impl (Emit l) = tell [l]

handleAuth :: (Text -> Bool) -> Eff (Authorization ': effects) a -> Eff effects a
handleAuth authz = interpret $ \(Verify prin) -> pure (authz prin)

handleCrypto :: Text -> Eff (Crypto ': effects) a -> Eff effects a
handleCrypto salt = interpret $ \Salt -> pure salt

handleClock :: UTCTime -> Eff (Clock ': effects) a -> Eff effects a
handleClock now = interpret $ \Now -> pure now

denied :: (Text -> Bool) -> Eff '[Clock, Crypto, DualControlEventStream, Authorization] (Maybe Text) -> Assertion
denied authz es = (fst $ runOp "whatever" authz epoch es) === Nothing

granted :: (Text -> Bool) -> Eff '[Clock, Crypto, DualControlEventStream, Authorization] (Maybe Text) -> Assertion
granted authz es =
    let salt = "salty"
        response = fst $ runOp salt authz epoch es
    in response === Just salt

type Principal = Text

grantedTo :: Eff '[Clock, Crypto, DualControlEventStream, Authorization] (Maybe Token) -> (Principal -> Bool) -> HashSet Text
grantedTo es authz = maybe (fromList []) principals (fst (runOp "salt" authz epoch es))

grantedUntil :: Eff '[Clock, Crypto, DualControlEventStream, Authorization] (Maybe Token) -> (Principal -> Bool) -> UTCTime -> UTCTime
grantedUntil es authz now = maybe epoch expiration (fst (runOp "salt" authz now es))

grantedFor :: Eff '[Clock, Crypto, DualControlEventStream, Authorization] (Maybe Token) -> (Principal -> Bool) -> Text
grantedFor es authz = maybe "" reason (fst (runOp "salt" authz epoch es))

notGranted :: Eff '[Clock, Crypto, DualControlEventStream, Authorization] (Maybe Token) -> (Principal -> Bool) -> Assertion
notGranted es authz = ( isJust ) (fst (runOp "salt" authz epoch es)) === False


logged :: [DualControlEvent] -> (Text -> Bool) -> Eff '[Clock, Crypto, DualControlEventStream, Authorization] a -> Assertion
logged expected authz es =
    let events = snd $ runOp "whatever" authz epoch es
        actual = filter (\e -> elem e expected) events
    in actual === expected

uniq :: (Ord a) => [a] -> [a]
uniq [] = []
uniq as = head <$> (group . sort) as
