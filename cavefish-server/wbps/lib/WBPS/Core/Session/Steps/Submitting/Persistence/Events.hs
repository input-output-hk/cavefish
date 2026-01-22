{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.Session.Steps.Submitting.Persistence.Events (
  EventHistory (..),
  loadHistory,
  load,
  persist,
) where

import Cardano.Api qualified as Api
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Path (Dir, Path, reldir, (</>))
import Path.IO (ensureDir)
import WBPS.Adapter.Monad.Control (whenNothingThrow)
import WBPS.Adapter.Path (readFrom, writeTo)
import WBPS.Core.Failure (WBPSFailure (BlindSignatureNotFound, SessionProofNotFound, SubmittedTxNotFound, TxSignatureNotFound))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.FetchAccounts (loadRegistered)
import WBPS.Core.Registration.Registered (Registered (Registered, userWalletPublicKey))
import WBPS.Core.Session.Persistence.FileScheme (deriveExistingSessionDirectoryFrom, deriveSessionDirectoryFrom)
import WBPS.Core.Session.SessionId (toSessionIdString)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (Commitment (Commitment, id), CommitmentId)
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated, commitment))
import WBPS.Core.Session.Steps.Demonstration.Persistence.Events qualified as Demonstrated
import WBPS.Core.Session.Steps.Proving.Persistence.Events qualified as Proved
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved)
import WBPS.Core.Session.Steps.Submitting.Artefacts.SubmittedTx (SubmittedTx (SubmittedTx))
import WBPS.Core.Session.Steps.Submitting.Submitted (
  CommitmentSubmitted (CommitmentSubmitted, blindSignature, submittedTx, txId, txSignature),
 )
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme

data EventHistory = EventHistory
  { registered :: Registered
  , demonstrated :: CommitmentDemonstrated
  , proved :: CommitmentProved
  , submitted :: CommitmentSubmitted
  }
  deriving (Eq, Show)

loadHistory ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey ->
  CommitmentId ->
  m EventHistory
loadHistory userWalletPublicKey commitmentId = do
  sessionDirectory <- deriveExistingSessionDirectoryFrom userWalletPublicKey commitmentId
  EventHistory
    <$> loadRegistered userWalletPublicKey
    <*> Demonstrated.load sessionDirectory userWalletPublicKey commitmentId
    <*> Proved.load sessionDirectory userWalletPublicKey commitmentId
    <*> load sessionDirectory userWalletPublicKey commitmentId

load ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  Path b Dir ->
  UserWalletPublicKey ->
  CommitmentId ->
  m CommitmentSubmitted
load sessionDirectory userWalletPublicKey commitmentId = do
  submitting <- asks (FileScheme.submitting . FileScheme.session . FileScheme.account)
  let submittedDirectory = sessionDirectory </> [reldir|submitted|]
  blindSignature <-
    readFrom (submittedDirectory </> FileScheme.blindSignature submitting)
      >>= whenNothingThrow [BlindSignatureNotFound (show userWalletPublicKey) (toSessionIdString commitmentId)]
  txSignature <-
    readFrom (submittedDirectory </> FileScheme.txSignature submitting)
      >>= whenNothingThrow [TxSignatureNotFound (show userWalletPublicKey) (toSessionIdString commitmentId)]
  submittedTx <-
    readFrom (submittedDirectory </> FileScheme.submittedTx submitting)
      >>= whenNothingThrow [SubmittedTxNotFound (show userWalletPublicKey) (toSessionIdString commitmentId)]
  let SubmittedTx (Api.Tx txBody _) = submittedTx
  let txId = Api.getTxId txBody
  pure CommitmentSubmitted {blindSignature, txSignature, submittedTx, txId}

persist ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  Registered ->
  CommitmentDemonstrated ->
  CommitmentSubmitted ->
  m CommitmentSubmitted
persist
  Registered {userWalletPublicKey}
  CommitmentDemonstrated {commitment = Commitment {id = commitmentId}}
  event@CommitmentSubmitted {blindSignature, txSignature, submittedTx} = do
    sessionDirectory <- deriveSessionDirectoryFrom userWalletPublicKey commitmentId
    ensureDir sessionDirectory
    submitting <- asks (FileScheme.submitting . FileScheme.session . FileScheme.account)
    let submittedDirectory = sessionDirectory </> [reldir|submitted|]
    writeTo (submittedDirectory </> FileScheme.blindSignature submitting) blindSignature
    writeTo (submittedDirectory </> FileScheme.txSignature submitting) txSignature
    writeTo (submittedDirectory </> FileScheme.submittedTx submitting) submittedTx
    return event
