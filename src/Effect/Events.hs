module Effect.Events (
    Event (..),
    Events (..),
    emitRequest,
    emitApprove,
    emitIssue,
    emitExpired
) where

import Control.Monad.Freer (Eff, Member, send)
import Types (Access, Principal, Reason, Event (..), Token)

data Events a where
    Emit :: Event -> Events ()

send' :: (Member Events effects) => Event -> Eff effects ()
send' = send . Emit

emitRequest :: (Member Events effects) => Access -> Principal -> Reason  -> Eff effects ()
emitRequest access principal reason = send' $ Request access principal reason

emitApprove :: (Member Events effects) => Token -> Principal -> Eff effects ()
emitApprove token principal = send' $ Approve token principal

emitIssue :: (Member Events effects) => Token -> Eff effects ()
emitIssue token = send' $ Issue token

emitExpired :: (Member Events effects) => Token -> Eff effects ()
emitExpired token = send' $ Expired token

