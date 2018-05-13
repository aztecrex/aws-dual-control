{-# LANGUAGE UndecidableInstances #-}
module AccessControl (
    requestAccess,
    approveAccess,
    tokenDurationSeconds,
) where

import AccessControl.Effect.Clock (Clock, now)
import AccessControl.Effect.Events (Events, emitRequest, emitApprove, emitIssue, emitExpired)
import AccessControl.Types (Access, Reason, Principal, Token (..))
import Control.Monad.Freer (Eff, Members)
import Data.HashSet (singleton)
import Data.Monoid ((<>))
import Data.Time.Clock (addUTCTime)

tokenDurationSeconds :: Integer
tokenDurationSeconds = 5400

-- | Make an initial access request. Depending on the resource and reason, this may be enough
--   to grant access.
requestAccess :: (Members '[Clock, Events] effects) =>
    -- | kind of access being requested
    Access ->
    -- | reason for access, use to document activity
    Reason ->
    -- | principal (user or otherwise) on whose behalf access is requested
    Principal ->
    -- | a token if successful request
    Eff effects (Maybe Token)
requestAccess access reason requestor = do
        emitRequest access requestor reason
        curTime <- now
        let token = Token access (singleton requestor) reason (addUTCTime (realToFrac tokenDurationSeconds) curTime)
        emitIssue token
        pure . Just $ token


-- | Approve an access request. For dual control and other cooperative controls.
approveAccess :: (Members '[Clock, Events] effects) =>
    -- | request token to approve
    Token ->
    -- | approver
    Principal ->
    -- | a token if approval succeeds
    Eff effects (Maybe Token)
approveAccess request approver = do
    emitApprove request approver
    curTime <- now
    if (expiration request) >= curTime
        then do
            let token = Token (access request) (singleton approver <> principals request) (reason request) (addUTCTime (realToFrac tokenDurationSeconds) curTime)
            emitIssue token
            pure . Just $ token
        else do
            emitExpired request
            pure Nothing

