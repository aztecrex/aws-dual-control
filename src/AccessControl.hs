{-# LANGUAGE UndecidableInstances #-}
module AccessControl (
    AccessControl (..)
) where

import AccessControl.Effect.Clock (Clock, now)
import AccessControl.Effect.Events (Events, emitRequest, emitApprove, emitIssue, emitExpired)
import AccessControl.Types (Access, Reason, Principal, Token (..))
import Control.Monad.Freer (Eff, Members)
import Data.HashSet (singleton)
import Data.Monoid ((<>))
import Data.Time.Clock (addUTCTime)


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
        emitRequest access requestor reason
        curTime <- now
        let token = Token access (singleton requestor) reason (addUTCTime (realToFrac (3600 :: Int)) curTime)
        emitIssue token
        pure . Just $ token
    approveAccess request approver = do
        emitApprove request approver
        curTime <- now
        if (expiration request) >= curTime
            then do
                let token = Token (access request) (singleton approver <> principals request) (reason request) (addUTCTime (realToFrac (3600 :: Int)) curTime)
                emitIssue token
                pure . Just $ token
            else do
                emitExpired request
                pure Nothing

