{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cavefish.Endpoints.Write.Session.Submit (handle, Inputs (..), Outputs (..)) where

import Cardano.Api (TxId)
import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, txBuildingService, wbpsService),
 )
import Cavefish.Services.TxBuilding qualified as TxService
import Cavefish.Services.WBPS qualified as WbpsService
import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Session.Steps.BlindSigning.BlindSignature (BlindSignature)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (CommitmentId)
import WBPS.Core.Session.Steps.Submitting.Artefacts.SubmittedTx (SubmittedTx (SubmittedTx))
import WBPS.Core.Session.Steps.Submitting.Submitted (CommitmentSubmitted (CommitmentSubmitted, submittedTx, txId))

data Inputs = Inputs
  { userWalletPublicKey :: UserWalletPublicKey
  , commitmentId :: CommitmentId
  , signature :: BlindSignature
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Outputs = Outputs
  { txId :: TxId
  }
  deriving newtype (Eq, Show, FromJSON, ToJSON)

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {userWalletPublicKey, commitmentId, signature} = do
  CavefishServices
    { wbpsService = WbpsService.WBPS {submit}
    , txBuildingService = TxService.TxBuilding {submit = submitTx}
    } <-
    ask
  submitted@CommitmentSubmitted {submittedTx = SubmittedTx tx} <- submit userWalletPublicKey commitmentId signature
  submitTx tx
  pure (toOutputs submitted)

toOutputs :: CommitmentSubmitted -> Outputs
toOutputs CommitmentSubmitted {txId} =
  Outputs {txId}
