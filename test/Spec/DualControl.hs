{-# LANGUAGE UndecidableInstances  #-}

module Spec.DualControl (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)

import Control.Monad.Freer (Eff, Members, send, run, reinterpret, interpret)
import Control.Monad.Freer.Writer (Writer, tell, runWriter)
import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.HashSet (fromList, singleton, HashSet)
import Data.List (elem)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..), addUTCTime)

(===) :: (Eq a, Show a) => a -> a -> Assertion
(===) = (@?=)
infix 1 ===

tests :: TestTree
tests = testGroup "DualControl" [

    testCase "init access" $ do
        let
        -- given
            access = AccountRole (Account "1234") (Role "giant")
            principal = Principal "Sonny"
            because = FightFire "I say so"
            now = UTCTime (toEnum 40001) 12

        -- when
            actual = requestAccess access because principal

        -- then
        accessOf actual === access
        principalsOf actual === fromList [principal]
        expirationOf actual now === addUTCTime (realToFrac (3600 :: Int)) now
        reasonOf actual === because
        contains (logsOf actual now) (Request access principal because)
        contains (logsOf actual now) (Issue (tokenOf actual now))
        ,

    testCase "approve access" $ do
        let
        -- given
            now = UTCTime (toEnum 40401) 1213
            currentAccess = AccountRole (Account "12") (Role "king")
            currentPrincipals = fromList [Principal "Sonny", Principal "Hope"]
            currentExpiration = addUTCTime (realToFrac (-1000 :: Int)) now
            currentReason = Develop

            token = Token currentAccess currentPrincipals currentReason currentExpiration
            principal = Principal "Cher"

        -- when
            actual = approveAccess token principal

        -- then
        accessOf actual === currentAccess
        principalsOf actual === fromList [principal] <> currentPrincipals
        expirationOf actual now === addUTCTime (realToFrac (3600 :: Int)) now -- an hour from now, irrespective of current
        reasonOf actual === currentReason
        contains (logsOf actual now) (Approve token principal)
        contains (logsOf actual now) (Issue (tokenOf actual now))

    --     ,



    -- testCase "single principal unauthorized result" $ do
    --     let
    --     -- given
    --         principal = "Sonny"
    --         reason = "I say so"

    --     -- when
    --         actual = grant' principal reason

    --     -- then
    --     notGranted actual (const False),

    -- testCase "single principal emit attempt and success event" $ do
    --     let
    --     -- when
    --         principal = "Cor"
    --         reason = "rain"
    --         actual = grant' principal reason

    --     -- then
    --     logged [
    --         (Attempt (fromList [principal]) reason),
    --         (Grant' (fromList [principal]) reason)
    --         ] (const True) actual,

    -- testCase "single principal unauthorized emit attempt and success event" $ do
    --     let
    --     -- when
    --         principal = "Cor"
    --         reason = "rain"
    --         actual = grant' principal reason

    --     -- then
    --     logged [
    --         (Attempt (fromList [principal]) reason),
    --         (Unauthorized (fromList [principal]) reason)
    --         ] (const False) actual,

    -- testCase "grant a token to two principals" $ do
    --     let
    --     -- given
    --         principal1 = "ace"
    --         principal2 = "tanya"
    --         principals = [principal1, principal2]

    --     -- when
    --         actual = grant principals "say so"

    --     -- then
    --     granted (const True) actual,

    -- testCase "grant a token to more than two principals" $ do
    --     let
    --     -- given
    --         principal1 = "charlie"
    --         principal2 = "amy"
    --         principal3 = "mostro"
    --         principals = [principal1, principal2, principal3]

    --     -- when
    --         actual = grant principals "crash"

    --     -- then
    --     granted (const True) actual,

    -- testCase "do not grant a token to 1 principal" $ do
    --     let
    --     -- given
    --         principal = "terry"
    --         principals = [principal]

    --     -- when
    --         actual = grant principals "overload"

    --     -- then
    --     denied (const True) actual,

    -- testCase "grant when reason provided" $ do
    --     let
    --     -- given
    --         reason = "on fire"
    --         principals = ["percival", "nona"]

    --     -- when
    --         actual = grant principals reason

    --     -- then
    --     granted (const True) actual,

    -- testCase "deny grant when reason is empty" $ do
    --     let
    --     -- given
    --         reason = ""
    --         principals = ["sheila", "thomas"]

    --     -- when
    --         actual = grant principals reason

    --     -- then
    --     denied (const True) actual,

    -- testCase "deny if fewer than 2 unique principals" $ do
    --     let
    --     -- given
    --         principal1 = "charlie"
    --         principals = [principal1, principal1, principal1]

    --     -- when
    --         actual = grant principals "crash"

    --     -- then
    --     denied (const True) actual,

    -- testCase "emit attempt event when granted" $ do
    --     let
    --         -- given
    --             principals = ["natalie", "christopher"]
    --             reason = "something happened"

    --         -- when
    --             actual = grant principals reason

    --         -- then
    --     logged [(Attempt (fromList principals) reason)] (const True) actual,

    -- testCase "emit attempt event when not" $ do
    --     let
    --     -- given
    --         principals = ["json"]
    --         reason = "something happened"

    --     -- when
    --         actual = grant principals reason

    --     -- then
    --     logged [(Attempt (fromList principals) reason)] (const True) actual,

    -- testCase "log principals and reason when granted" $ do
    --     let
    --     -- given
    --         principals = ["natalie", "christopher"]
    --         reason = "something happened"

    --     -- when
    --         actual = grant principals reason

    --     -- then
    --     logged [Grant (sort principals) reason] (const True) actual,

    -- testCase "log not-authorized authorization denied" $ do
    --     let
    --     -- given
    --         principals = ["natalie"]
    --         reason = "something happened"

    --     -- when
    --         actual = grant principals reason

    --     -- then
    --     logged [NotAuthorized  (sort principals) [] reason] (const True) actual,

    -- testCase "log missing-reason when missing reason" $ do
    --     let
    --     -- given
    --         principals = ["charlene", "toby", "carlton" ]
    --         reason = ""

    --     -- when
    --         actual = grant principals reason

    --     -- then
    --     logged [MissingReason (sort principals)] (const True) actual,

    -- testCase "denies unauthorized principals" $ do
    --     let
    --     -- given
    --         authorized = "josie"
    --         unauthorized = "tom"
    --         principals = [authorized, unauthorized]

    --     -- when
    --         actual = grant principals "rain"

    --     -- then
    --     denied (== authorized) actual

    ]


data Event where
    Request :: Access -> Principal -> Reason -> Event
    Approve :: Token -> Principal -> Event
    Issue :: Token -> Event
    deriving (Eq, Show)

data Events a where
    Emit :: Event -> Events ()

data Clock a where
    Now :: Clock UTCTime

-- class DualControl r where
--     grant :: [Text] -> Text -> r (Maybe Text)

-- type Principal = Text

-- instance (Members '[Clock, Crypto, DualControlEventStream, Authorization] effects) => DualControl (Eff effects) where
--     grant principals reason = do
--         send (Emit (Attempt (fromList principals) reason))
--         let unique = uniq principals
--         verify <- mapM (send . Verify) unique
--         if length (filter id verify) >= 2
--             then if (T.null reason)
--                     then do
--                         send (Emit (MissingReason (sort principals)))
--                         pure Nothing
--                     else do
--                         send (Emit (Grant (sort principals) reason))
--                         salt <- send Salt
--                         pure (Just salt)
--             else do
--                 send (Emit (NotAuthorized (sort principals) [] reason))
--                 pure Nothing


newtype Principal = Principal Text
    deriving (Eq, Show, Hashable)

data Reason where
    Provision :: Reason
    Deploy :: Reason
    Monitor :: Reason
    FightFire :: Text -> Reason
    Emergency :: Text -> Reason
    Administer :: Text -> Reason
    Develop :: Reason
    deriving (Eq, Show)

newtype Account = Account Text deriving (Eq, Show, Hashable)
newtype Role = Role Text deriving (Eq, Show, Hashable)

data Access where
    AccountRole :: Account -> Role -> Access
    deriving (Eq, Show)

data Token = Token {
    access :: Access,
    principals :: HashSet Principal,
    reason :: Reason,
    expiration :: UTCTime
} deriving (Eq, Show)

class AccessControl r where
    -- | Make an initial access request. Depending on the resource and reason, this may be enough
    --   to grant access.
    requestAccess ::
        -- | kind of access being requested
        Access ->
        -- | reason for access, use to document activity
        Reason ->
        -- | principal (user or otherwise) on whose behalf access is requested
        Principal ->
        -- | a token if successful request
        r (Maybe Token)

    -- | Approve an access request. For dual control and other cooperative controls.
    approveAccess ::
        -- | request token to approve
        Token ->
        -- | approver
        Principal ->
        -- | a token if approval succeeds
        r (Maybe Token)



instance (Members '[Clock, Events] effects) => AccessControl (Eff effects) where
    requestAccess access reason requestor = do
        send (Emit (Request access requestor reason ))
        now <- send Now
        let token = Token access (singleton requestor) reason (addUTCTime (realToFrac (3600 :: Int)) now)
        send (Emit (Issue token))
        pure . Just $ token
    approveAccess request approver = do
        send (Emit (Approve request approver))
        now <- send Now
        let token = Token (access request) (singleton approver <> principals request) (reason request) (addUTCTime (realToFrac (3600 :: Int)) now)
        send (Emit (Issue token))
        pure . Just $ token


contains :: (Eq a) => [a] -> a ->  Assertion
contains as a = elem a as === True

epoch :: UTCTime
epoch =(UTCTime (toEnum 0) 0)

runOp :: UTCTime -> Eff '[Clock, Events] a -> (a, [Event])
runOp now es =
    handleClock now es & handleEvents & run

handleEvents :: Eff (Events ': effects) a -> Eff effects (a, [Event])
handleEvents es = runWriter $ reinterpret impl es
    where
        impl :: Events b -> Eff (Writer [Event] ': effs) b
        impl (Emit l) = tell [l]

handleClock :: UTCTime -> Eff (Clock ': effects) a -> Eff effects a
handleClock now = interpret $ \Now -> pure now

tokenOf :: Eff '[Clock, Events] (Maybe Token) -> UTCTime -> Token
tokenOf es now = fromJust . fst $ runOp now es

accessOf :: Eff '[Clock, Events] (Maybe Token) -> Access
accessOf es = access . fromJust . fst $ runOp epoch es

expirationOf :: Eff '[Clock, Events] (Maybe Token) -> UTCTime -> UTCTime
expirationOf es now = expiration . fromJust . fst $ runOp now es

reasonOf :: Eff '[Clock, Events] (Maybe Token) -> Reason
reasonOf es =  reason . fromJust . fst $ runOp epoch es

principalsOf :: Eff '[Clock, Events] (Maybe Token) -> HashSet Principal
principalsOf es = principals . fromJust . fst $ runOp epoch es

logsOf :: Eff '[Clock, Events] (Maybe Token) -> UTCTime -> [Event]
logsOf es now = snd $ runOp now es
