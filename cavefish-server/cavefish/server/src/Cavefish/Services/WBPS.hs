module Cavefish.Services.WBPS (
  WBPS (..),
) where

import Cardano.Api (
  MonadError,
  MonadIO,
 )
import Servant.Server.Internal.ServerError (ServerError)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Registered (Registered)
import WBPS.Core.Session.Session (Session)
import WBPS.Core.Session.Steps.BlindSigning.BlindSignature (BlindSignature)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (CommitmentId)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (CommitmentDemonstrated)
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved)
import WBPS.Core.Session.Steps.Submitting.Submitted (CommitmentSubmitted)

data WBPS = WBPS
  { register ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey ->
      m Registered
  , demonstrate ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> UnsignedTx -> m CommitmentDemonstrated
  , prove ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> CommitmentId -> R -> m CommitmentProved
  , submit ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> CommitmentId -> BlindSignature -> m CommitmentSubmitted
  , loadRegisteredMaybe ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> m (Maybe Registered)
  , loadAllRegistered ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      m [Registered]
  , loadSession ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> CommitmentId -> m Session
  , loadCommitmentDemonstrationEvents ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> CommitmentId -> m (Registered, CommitmentDemonstrated)
  }
