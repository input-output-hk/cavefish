module WBPS.Core.Session.SessionId (
  SessionId (..),
  deriveId,
  ToSessionId (..),
  toSessionIdString,
) where

import Data.Text qualified as Text
import WBPS.Adapter.CardanoCryptoClass.Crypto (Codec (encode))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (CommitmentId (CommitmentId))

newtype SessionId = SessionId {unSessionId :: String} deriving (Show, Eq)

deriveId :: CommitmentId -> SessionId
deriveId (CommitmentId x) = SessionId . Text.unpack . encode $ x

class ToSessionId a where
  toSessionId :: a -> SessionId

instance ToSessionId CommitmentId where
  toSessionId = deriveId

instance ToSessionId SessionId where
  toSessionId = id

toSessionIdString :: ToSessionId a => a -> String
toSessionIdString = unSessionId . toSessionId
