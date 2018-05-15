module AccessControl.Types (
    Principal (..),
    Reason (..),
    Account (..),
    Role (..),
    Access (..),
    Token (..),
    Event (..),
    AccessCredentials
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
newtype HostCoordinates = HostCoordinates Text deriving (Eq, Show, Hashable)

data Access where
    AccountRole :: Account -> Role -> Access
    AccountRoot :: Account -> Access
    AccountHost :: Account -> HostCoordinates -> Access
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

newtype AccessKey = AccessKey Text deriving (Eq, Show, Hashable)
newtype SecretKey = SecretKey Text deriving (Eq, Show, Hashable)
newtype SessionKey = SessionKey Text deriving (Eq, Show, Hashable)
newtype Username = Username Text deriving (Eq, Show, Hashable)
newtype Password = Password Text deriving (Eq, Show, Hashable)
newtype SSHKey = SSHKey Text deriving (Eq, Show, Hashable)

data AWSSession where
    AWSSession :: AccessKey -> SecretKey -> SessionKey -> AWSSession
    deriving (Eq, Show)

data AWSAccountRoot where
    AWSAccountRoot :: Username -> Password -> AWSAccountRoot
    deriving (Eq, Show)

data SSHHost where
    SSHHost :: Username -> SSHKey -> SSHHost
    deriving (Eq, Show)

data AccessCredentials where
