{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.Session.Steps.Demonstration.Persistence.Events (
  EventHistory (..),
  loadHistory,
  load,
  persist,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Path (Dir, Path, reldir, (</>))
import Path.IO (ensureDir)
import WBPS.Adapter.Monad.Control (whenNothingThrow)
import WBPS.Adapter.Path (readFrom, writeTo)
import WBPS.Core.Failure (WBPSFailure (EncryptionKeysNotFound, SessionMessageNotFound))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.FetchAccounts (loadRegistered)
import WBPS.Core.Registration.Registered (Registered (Registered, userWalletPublicKey))
import WBPS.Core.Session.Persistence.FileScheme (deriveExistingSessionDirectoryFrom, deriveSessionDirectoryFrom)
import WBPS.Core.Session.SessionId (toSessionIdString)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (Commitment (Commitment, id), CommitmentId)
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (
  CommitmentDemonstrated (
    CommitmentDemonstrated,
    commitment,
    preparedMessage,
    scalars
  ),
 )
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme

data EventHistory = EventHistory
  { registered :: Registered
  , demonstrated :: CommitmentDemonstrated
  }
  deriving (Eq, Show)

loadHistory ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> CommitmentId -> m EventHistory
loadHistory userWalletPublicKey commitmentId = do
  sessionDirectory <- deriveExistingSessionDirectoryFrom userWalletPublicKey commitmentId
  EventHistory
    <$> loadRegistered userWalletPublicKey
    <*> load sessionDirectory userWalletPublicKey commitmentId

load ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  Path b Dir ->
  UserWalletPublicKey ->
  CommitmentId ->
  m CommitmentDemonstrated
load sessionDirectory userWalletPublicKey commitmentId = do
  demonstration <- asks (FileScheme.demonstration . FileScheme.session . FileScheme.account)
  let demonstratedDirectory = sessionDirectory </> [reldir|demonstrated|]
  CommitmentDemonstrated
    <$> ( readFrom (demonstratedDirectory </> FileScheme.preparedMessage demonstration)
            >>= whenNothingThrow [SessionMessageNotFound (show userWalletPublicKey) (toSessionIdString commitmentId)]
        )
    <*> ( readFrom (demonstratedDirectory </> FileScheme.scalars demonstration)
            >>= whenNothingThrow [EncryptionKeysNotFound (show userWalletPublicKey)]
        )
    <*> ( readFrom (demonstratedDirectory </> FileScheme.commitment demonstration)
            >>= whenNothingThrow [EncryptionKeysNotFound (show userWalletPublicKey)]
        )

persist ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  Registered -> CommitmentDemonstrated -> m CommitmentDemonstrated
persist
  Registered {userWalletPublicKey}
  event@CommitmentDemonstrated
    { preparedMessage
    , scalars
    , commitment = commitment@Commitment {id = sessionId}
    } = do
    sessionDirectory <- deriveSessionDirectoryFrom userWalletPublicKey sessionId
    ensureDir sessionDirectory
    demonstration <- asks (FileScheme.demonstration . FileScheme.session . FileScheme.account)
    writeTo (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.preparedMessage demonstration) preparedMessage
    writeTo (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.scalars demonstration) scalars
    writeTo (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.commitment demonstration) commitment
    return event
