{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.Session.Steps.Proving.Persistence.Events (
  EventHistory (..),
  loadHistory,
  load,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Path (Dir, Path, reldir, (</>))
import WBPS.Adapter.Monad.Control (whenNothingThrow)
import WBPS.Adapter.Path (readFrom)
import WBPS.Core.Failure (WBPSFailure (SessionProofNotFound))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.FetchAccounts (loadRegistered)
import WBPS.Core.Registration.Registered (Registered)
import WBPS.Core.Session.Persistence.FileScheme (deriveExistingSessionDirectoryFrom)
import WBPS.Core.Session.SessionId (toSessionIdString)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (CommitmentId)
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (CommitmentDemonstrated)
import WBPS.Core.Session.Steps.Demonstration.Persistence.Events qualified as Demonstrated
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved (CommitmentProved))
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme

data EventHistory = EventHistory
  { registered :: Registered
  , demonstrated :: CommitmentDemonstrated
  , proved :: CommitmentProved
  }
  deriving (Eq, Show)

loadHistory ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> CommitmentId -> m EventHistory
loadHistory userWalletPublicKey commitmentId = do
  sessionDirectory <- deriveExistingSessionDirectoryFrom userWalletPublicKey commitmentId
  EventHistory
    <$> loadRegistered userWalletPublicKey
    <*> Demonstrated.load sessionDirectory userWalletPublicKey commitmentId
    <*> load sessionDirectory userWalletPublicKey commitmentId

load ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  Path b Dir ->
  UserWalletPublicKey ->
  CommitmentId ->
  m CommitmentProved
load sessionDirectory userWalletPublicKey commitmentId = do
  proving <- asks (FileScheme.proving . FileScheme.session . FileScheme.account)
  let FileScheme.Proving
        { bigR = bigRFile
        , challenge = challengeFile
        , proof = FileScheme.ProofGeneration {proof = proofFile}
        } = proving
  let provedDirectory = sessionDirectory </> [reldir|proved|]
  CommitmentProved
    <$> ( readFrom (provedDirectory </> bigRFile)
            >>= whenNothingThrow [SessionProofNotFound (show userWalletPublicKey) (toSessionIdString commitmentId)]
        )
    <*> ( readFrom (provedDirectory </> challengeFile)
            >>= whenNothingThrow [SessionProofNotFound (show userWalletPublicKey) (toSessionIdString commitmentId)]
        )
    <*> ( readFrom (provedDirectory </> proofFile)
            >>= whenNothingThrow [SessionProofNotFound (show userWalletPublicKey) (toSessionIdString commitmentId)]
        )
