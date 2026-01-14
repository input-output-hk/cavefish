module WBPS.Core.Session.Proving.Prove (
  prove,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import WBPS.Core.Failure (WBPSFailure)
import WBPS.Core.FileScheme (
  FileScheme,
 )
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Session.Demonstration.Commitment (CommitmentId)
import WBPS.Core.Session.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated, preparedMessage))
import WBPS.Core.Session.Demonstration.PreparedMessage (MessageParts (MessageParts, message), PreparedMessage (PreparedMessage, parts))
import WBPS.Core.Session.Demonstration.R (R)
import WBPS.Core.Session.FetchSession (loadExistingCommitmentDemonstrationEvents)
import WBPS.Core.Session.Proving.Challenge qualified as Challenge
import WBPS.Core.Session.Proving.Proof.Generate (generateProof)
import WBPS.Core.Session.Proving.Proved (CommitmentProved (CommitmentProved, bigR, challenge, proof))
import WBPS.Core.Session.Proving.Witness qualified as Witness (generate)

prove ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey ->
  CommitmentId ->
  R ->
  m CommitmentProved
prove userWalletPublicKey commitmentId bigR = do
  (registered, commitmentDemonstrated@CommitmentDemonstrated {preparedMessage = PreparedMessage {parts = MessageParts {message}}}) <-
    loadExistingCommitmentDemonstrationEvents userWalletPublicKey commitmentId
  let challenge = Challenge.computeByUsingTxId userWalletPublicKey message bigR
  Witness.generate registered commitmentDemonstrated bigR challenge
  proof <- generateProof userWalletPublicKey commitmentId
  return CommitmentProved {..}
