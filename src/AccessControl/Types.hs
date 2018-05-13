module AccessControl.Types (
    Principal (..),
    Reason (..),
    Account (..),
    Role (..),
    Access (..),
    Token (..),
    Event (..)
) where

import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

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
    AccountRoot :: Account -> Access
    AccountHost :: Account -> Access
    deriving (Eq, Show)

data Token = Token {
    access :: Access,
    principals :: HashSet Principal,
    reason :: Reason,
    expiration :: UTCTime
} deriving (Eq, Show)

data Event where
    Request :: Access -> Principal -> Reason -> Event
    Approve :: Token -> Principal -> Event
    Issue :: Token -> Event
    Expired :: Token -> Event
    deriving (Eq, Show)

