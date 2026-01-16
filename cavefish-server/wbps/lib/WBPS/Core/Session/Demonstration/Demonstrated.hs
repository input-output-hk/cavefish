module WBPS.Core.Session.Demonstration.Demonstrated (
  CommitmentDemonstrated (..),
) where

import GHC.Generics (Generic)
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (Commitment)
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage (PreparedMessage)
import WBPS.Core.Session.Demonstration.Artefacts.Scalars (Scalars)

data CommitmentDemonstrated
  = CommitmentDemonstrated
  { preparedMessage :: PreparedMessage
  , scalars :: Scalars
  , commitment :: Commitment
  }
  deriving (Eq, Show, Generic)
