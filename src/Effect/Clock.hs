module Effect.Clock (
    Clock (..),
    now
) where

import Control.Monad.Freer (Member, Eff, send)
import Data.Time.Clock (UTCTime)

data Clock a where
    Now :: Clock UTCTime

now :: (Member Clock effects) => Eff effects UTCTime
now = send Now
