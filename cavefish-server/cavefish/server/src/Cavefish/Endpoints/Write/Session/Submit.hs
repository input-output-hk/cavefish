{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cavefish.Endpoints.Write.Session.Submit (handle, Inputs (..), Outputs (..)) where

import Cardano.Api (TxId)
import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, wbpsService),
 )
import Cavefish.Services.WBPS qualified as WbpsService
import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Session.Steps.BlindSigning.Sign (BlindSignature)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (CommitmentId)
import WBPS.Core.Session.Steps.Submitting.Submitted (CommitmentSubmitted (CommitmentSubmitted, txId))

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
  CavefishServices {wbpsService = WbpsService.WBPS {submit}} <- ask
  toOutputs <$> submit userWalletPublicKey commitmentId signature

toOutputs :: CommitmentSubmitted -> Outputs
toOutputs CommitmentSubmitted {txId} =
  Outputs {txId}
