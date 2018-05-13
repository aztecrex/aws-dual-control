module Spec.AccessControl (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)

import Control.Monad.Freer (Eff, run, reinterpret, interpret)
import Control.Monad.Freer.Writer (Writer, tell, runWriter)
import Data.Function ((&))
import Data.HashSet (fromList, HashSet)
import Data.List (elem)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime (..), addUTCTime)

import AccessControl (requestAccess, approveAccess)
import AccessControl.Effect.Clock
import AccessControl.Effect.Events (Event (..), Events (..))
import AccessControl.Types

(===) :: (Eq a, Show a) => a -> a -> Assertion
(===) = (@?=)
infix 1 ===

tests :: TestTree
tests = testGroup "DualControl" [

    testCase "request token contains access" $ do
        let
        -- given
            access = AccountRole (Account "1234") (Role "giant")
            principal = Principal "Sonny"
            because = FightFire "I say so"
            curTime = UTCTime (toEnum 40001) 12

        -- when
            actual = requestAccess access because principal

        -- then
        accessOf actual curTime === access,

    testCase "request token contains principal" $ do
        let
        -- given
            access = AccountRole (Account "1234") (Role "giant")
            principal = Principal "Sonny"
            because = FightFire "I say so"
            curTime = UTCTime (toEnum 40001) 12

        -- when
            actual = requestAccess access because principal

        -- then
        principalsOf actual curTime === fromList [principal],

    testCase "request token expires in 1 hour" $ do
        let
        -- given
            access = AccountRole (Account "1234") (Role "giant")
            principal = Principal "Sonny"
            because = FightFire "I say so"
            curTime = UTCTime (toEnum 40001) 12

        -- when
            actual = requestAccess access because principal

        -- then
        expirationOf actual curTime === addUTCTime (realToFrac (3600 :: Int)) curTime,

    testCase "request token contains reason" $ do
        let
        -- given
            access = AccountRole (Account "1234") (Role "giant")
            principal = Principal "Sonny"
            because = FightFire "I say so"
            curTime = UTCTime (toEnum 40001) 12

        -- when
            actual = requestAccess access because principal

        -- then
        reasonOf actual curTime === because,

    testCase "request logs attempt" $ do
        let
        -- given
            access = AccountRole (Account "1234") (Role "giant")
            principal = Principal "Sonny"
            because = FightFire "I say so"
            curTime = UTCTime (toEnum 40001) 12

        -- when
            actual = requestAccess access because principal

        -- then
        contains (logsOf actual curTime) (Request access principal because),

    testCase "request logs issue" $ do
        let
        -- given
            access = AccountRole (Account "1234") (Role "giant")
            principal = Principal "Sonny"
            because = FightFire "I say so"
            curTime = UTCTime (toEnum 40001) 12

        -- when
            actual = requestAccess access because principal

        -- then
        contains (logsOf actual curTime) (Issue (tokenOf actual curTime)),

    testCase "approve carries access" $ do
        let
        -- given
            curTime = UTCTime (toEnum 40401) 1213
            currentAccess = AccountRole (Account "12") (Role "king")
            currentPrincipals = fromList [Principal "Sonny", Principal "Hope"]
            currentExpiration = addUTCTime (realToFrac (1000 :: Int)) curTime
            currentReason = Develop

            token = Token currentAccess currentPrincipals currentReason currentExpiration
            principal = Principal "Cher"

        -- when
            actual = approveAccess token principal

        -- then
        accessOf actual curTime === currentAccess,

    testCase "approve adds principal" $ do
        let
        -- given
            curTime = UTCTime (toEnum 40401) 1213
            currentAccess = AccountRole (Account "12") (Role "king")
            currentPrincipals = fromList [Principal "Sonny", Principal "Hope"]
            currentExpiration = addUTCTime (realToFrac (1100 :: Int)) curTime
            currentReason = Develop

            token = Token currentAccess currentPrincipals currentReason currentExpiration
            principal = Principal "Cher"

        -- when
            actual = approveAccess token principal

        -- then
        principalsOf actual curTime === fromList [principal] <> currentPrincipals,

    testCase "approve resets expiration to one hour from curTime" $ do
        let
        -- given
            curTime = UTCTime (toEnum 40401) 1213
            currentAccess = AccountRole (Account "12") (Role "king")
            currentPrincipals = fromList [Principal "Sonny", Principal "Hope"]
            currentExpiration = addUTCTime (realToFrac (1201 :: Int)) curTime
            currentReason = Develop

            token = Token currentAccess currentPrincipals currentReason currentExpiration
            principal = Principal "Cher"

        -- when
            actual = approveAccess token principal

        -- then
        expirationOf actual curTime === addUTCTime (realToFrac (3600 :: Int)) curTime, -- an hour from curTime, irrespective of current

    testCase "approve carries reason" $ do
        let
        -- given
            curTime = UTCTime (toEnum 40401) 1213
            currentAccess = AccountRole (Account "12") (Role "king")
            currentPrincipals = fromList [Principal "Sonny", Principal "Hope"]
            currentExpiration = addUTCTime (realToFrac (1000 :: Int)) curTime
            currentReason = Develop

            token = Token currentAccess currentPrincipals currentReason currentExpiration
            principal = Principal "Cher"

        -- when
            actual = approveAccess token principal

        -- then
        reasonOf actual curTime === currentReason,

    testCase "approve emits attempt" $ do
        let
        -- given
            curTime = UTCTime (toEnum 40401) 1213
            currentAccess = AccountRole (Account "12") (Role "king")
            currentPrincipals = fromList [Principal "Sonny", Principal "Hope"]
            currentExpiration = addUTCTime (realToFrac (1001 :: Int)) curTime
            currentReason = Develop

            token = Token currentAccess currentPrincipals currentReason currentExpiration
            principal = Principal "Cher"

        -- when
            actual = approveAccess token principal

        -- then
        contains (logsOf actual curTime) (Approve token principal),

    testCase "approve emits issue" $ do
        let
        -- given
            curTime = UTCTime (toEnum 40401) 1213
            currentAccess = AccountRole (Account "12") (Role "king")
            currentPrincipals = fromList [Principal "Sonny", Principal "Hope"]
            currentExpiration = addUTCTime (realToFrac (1002 :: Int)) curTime
            currentReason = Develop

            token = Token currentAccess currentPrincipals currentReason currentExpiration
            principal = Principal "Cher"

        -- when
            actual = approveAccess token principal

        -- then
        contains (logsOf actual curTime) (Issue (tokenOf actual curTime)),

    testCase "approve fails when expired" $ do
        let
        -- given
            curTime = UTCTime (toEnum 42123) 1703
            currentExpiration = addUTCTime (realToFrac (-3601 :: Int)) curTime
            token = Token  (AccountRole (Account "13") (Role "duke")) (fromList [Principal "Ace"]) Deploy currentExpiration

        -- when
            actual = approveAccess token (Principal "Jones")

        -- then
        succeeded actual curTime === False,

    testCase "expired approve emits attempt" $ do
        let
        -- given
            curTime = UTCTime (toEnum 42123) 1703
            principal = Principal "Penelope"
            currentExpiration = addUTCTime (realToFrac (-3601 :: Int)) curTime
            token = Token  (AccountRole (Account "13") (Role "duke")) (fromList [Principal "Ace"]) Deploy currentExpiration

        -- when
            actual = approveAccess token principal

        -- then
        contains (logsOf actual curTime) (Approve token principal),

    testCase "expired approval emits expired" $ do
        let
        -- given
            curTime = UTCTime (toEnum 42123) 1703
            currentExpiration = addUTCTime (realToFrac (-3601 :: Int)) curTime
            token = Token  (AccountRole (Account "13") (Role "duke")) (fromList [Principal "Ace"]) Deploy currentExpiration

        -- when
            actual = approveAccess token (Principal "Jones")

        -- then
        contains (logsOf actual curTime) (Expired token)


    ]


contains :: (Eq a) => [a] -> a ->  Assertion
contains as a = elem a as === True

runOp :: UTCTime -> Eff '[Clock, Events] a -> (a, [Event])
runOp curTime es =
    handleClock curTime es & handleEvents & run

handleEvents :: Eff (Events ': effects) a -> Eff effects (a, [Event])
handleEvents es = runWriter $ reinterpret impl es
    where
        impl :: Events b -> Eff (Writer [Event] ': effs) b
        impl (Emit l) = tell [l]

handleClock :: UTCTime -> Eff (Clock ': effects) a -> Eff effects a
handleClock curTime = interpret $ \Now -> pure curTime

tokenOf :: Eff '[Clock, Events] (Maybe Token) -> UTCTime -> Token
tokenOf es curTime = fromJust . fst $ runOp curTime es

accessOf :: Eff '[Clock, Events] (Maybe Token) -> UTCTime -> Access
accessOf es curTime = access . fromJust . fst $ runOp curTime es

expirationOf :: Eff '[Clock, Events] (Maybe Token) -> UTCTime -> UTCTime
expirationOf es curTime = expiration . fromJust . fst $ runOp curTime es

reasonOf :: Eff '[Clock, Events] (Maybe Token) -> UTCTime -> Reason
reasonOf es curTime =  reason . fromJust . fst $ runOp curTime es

principalsOf :: Eff '[Clock, Events] (Maybe Token) -> UTCTime -> HashSet Principal
principalsOf es curTime = principals . fromJust . fst $ runOp curTime es

logsOf :: Eff '[Clock, Events] (Maybe Token) -> UTCTime -> [Event]
logsOf es curTime = snd $ runOp curTime es

succeeded :: Eff '[Clock, Events] (Maybe Token) -> UTCTime -> Bool
succeeded es curTime = isJust . fst $ runOp curTime es
