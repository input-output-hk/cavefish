module WBPS.Core.Session.Steps.Submitting.Submitted (
  CommitmentSubmitted (..),
) where

import Cardano.Api (TxId)
import GHC.Generics (Generic)

newtype CommitmentSubmitted
  = CommitmentSubmitted
  { txId :: TxId
  }
  deriving (Eq, Show, Generic)
