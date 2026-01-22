module WBPS.Core.Session.Steps.Proving.Prove (
  prove,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import WBPS.Core.Failure (WBPSFailure)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Registered (Registered)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (CommitmentId)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (
  CircuitMessage (CircuitMessage, message),
  MessageBits,
  PreparedMessage (PreparedMessage, circuit),
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated, preparedMessage))
import WBPS.Core.Session.Steps.Demonstration.Persistence.Events qualified as Demonstrated
import WBPS.Core.Session.Steps.Demonstration.Persistence.Events qualified as Previous (EventHistory (EventHistory))
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge qualified as Challenge
import WBPS.Core.Session.Steps.Proving.Artefacts.Proof.Generate (generateProof)
import WBPS.Core.Session.Steps.Proving.Artefacts.Witness qualified as Witness (generate)
import WBPS.Core.Session.Steps.Proving.Persistence.Events (EventHistory (..))
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved (CommitmentProved, bigR, challenge, proof))
import WBPS.Core.Setup.Circuit.FileScheme (
  FileScheme,
 )

prove ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey ->
  CommitmentId ->
  R ->
  m EventHistory
prove userWalletPublicKey commitmentId bigR = do
  (registered, demonstrated, messageBits) <- project userWalletPublicKey commitmentId
  let challenge = Challenge.compute userWalletPublicKey messageBits bigR
  Witness.generate registered demonstrated bigR challenge
  proof <- generateProof userWalletPublicKey commitmentId
  return $
    EventHistory
      registered
      demonstrated
      CommitmentProved {..}

project ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey ->
  CommitmentId ->
  m (Registered, CommitmentDemonstrated, MessageBits)
project userWalletPublicKey commitmentId = do
  Previous.EventHistory
    { registered
    , demonstrated =
      demonstrated@CommitmentDemonstrated
        { preparedMessage =
          PreparedMessage
            { circuit = CircuitMessage {message = messageBits}
            }
        }
    } <-
    Demonstrated.loadHistory userWalletPublicKey commitmentId
  return (registered, demonstrated, messageBits)
