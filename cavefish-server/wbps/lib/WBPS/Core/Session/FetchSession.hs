-- | Module for fetching and loading user accounts from the file system.
-- This module provides functions to load existing accounts, load a specific account,
-- and retrieve all recorded user wallet public keys. It handles errors related to
-- missing encryption keys and uses a file scheme for directory structure.
module WBPS.Core.Session.FetchSession (
  loadSession,
  loadExistingSession,
  loadSessions,
  -- | Load an existing session
) where

import Control.Monad (join)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Data.Functor ((<&>))
import Data.String (fromString)
import Path (Dir, Path, toFilePath, (</>))
import Path.IO (doesDirExist, listDirRel)
import WBPS.Adapter.Monad.Control (ifM)
import WBPS.Core.Failure (WBPSFailure)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.FetchAccounts (loadAllRegistered, loadRegistered)
import WBPS.Core.Registration.Persistence.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Registration.Registered (Registered (Registered, userWalletPublicKey))
import WBPS.Core.Session.Persistence.FileScheme (deriveExistingSessionDirectoryFrom)
import WBPS.Core.Session.Session (Session (Demonstrated))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (CommitmentId)
import WBPS.Core.Session.Steps.Demonstration.Persistence.Events qualified as Demonstrated
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme

getRecordedCommitmentIds :: MonadIO m => Path b Dir -> m [CommitmentId]
getRecordedCommitmentIds p = do
  a <- fst <$> (liftIO . listDirRel $ p)
  return $ fromString . takeWhile (/= '/') . toFilePath <$> a

loadSessions ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) => m [Session]
loadSessions = do
  ( loadAllRegistered
      >>= mapM
        ( \Registered {userWalletPublicKey} -> do
            accountDir <- deriveAccountDirectoryFrom userWalletPublicKey
            FileScheme.Account {sessions} <- asks FileScheme.account
            recordedIds <- getRecordedCommitmentIds (accountDir </> sessions)
            traverse (loadExistingSession userWalletPublicKey) recordedIds
        )
    )
    <&> join

loadSession ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> CommitmentId -> m (Maybe Session)
loadSession userWalletPublicKey commitmentId = do
  account <- deriveAccountDirectoryFrom userWalletPublicKey
  ifM
    (not <$> doesDirExist account)
    (return Nothing)
    (Just <$> loadExistingSession userWalletPublicKey commitmentId)

loadExistingSession ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> CommitmentId -> m Session
loadExistingSession userWalletPublicKey commitmentId = do
  sessionDirectory <- deriveExistingSessionDirectoryFrom userWalletPublicKey commitmentId
  registered <- loadRegistered userWalletPublicKey
  demonstrated <- Demonstrated.load sessionDirectory userWalletPublicKey commitmentId
  pure (Demonstrated Demonstrated.EventHistory {registered, demonstrated})
