module WBPS.Core.Session.Steps.Proving.Prove (
  prove,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import WBPS.Core.Failure (WBPSFailure)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Session.FetchSession (loadExistingCommitmentDemonstrationEvents)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (CommitmentId)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (
  CircuitMessage (CircuitMessage, message),
  PreparedMessage (PreparedMessage, circuit),
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated, preparedMessage))
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge qualified as Challenge
import WBPS.Core.Session.Steps.Proving.Artefacts.Proof.Generate (generateProof)
import WBPS.Core.Session.Steps.Proving.Artefacts.Witness qualified as Witness (generate)
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved (CommitmentProved, bigR, challenge, proof))
import WBPS.Core.Setup.Circuit.FileScheme (
  FileScheme,
 )

prove ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey ->
  CommitmentId ->
  R ->
  m CommitmentProved
prove userWalletPublicKey commitmentId bigR = do
  (registered, commitmentDemonstrated@CommitmentDemonstrated {preparedMessage = PreparedMessage {circuit = CircuitMessage {message = messageBits}}}) <-
    loadExistingCommitmentDemonstrationEvents userWalletPublicKey commitmentId
  let challenge = Challenge.compute userWalletPublicKey messageBits bigR
  Witness.generate registered commitmentDemonstrated bigR challenge
  proof <- generateProof userWalletPublicKey commitmentId
  return CommitmentProved {..}
