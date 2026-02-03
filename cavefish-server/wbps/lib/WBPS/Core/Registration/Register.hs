{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module      : WBPS.Core.Registration.Register
--     Description : Functionality to register a new user account within the WBPS system.
module WBPS.Core.Registration.Register (
  register,
  -- | Register a new user account
) where

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Path (reldir, (</>))
import Path.IO (ensureDir)
import Shh (Stream (Append, StdOut), (&!>), (&>))
import WBPS.Adapter.Path (writeTo)
import WBPS.Core.Failure (WBPSFailure (AccountAlreadyRegistered))
import WBPS.Core.Performance (withPerfEventIO)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Artefacts.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Registration.FetchAccounts (loadExistingRegistered, loadRegisteredMaybe)
import WBPS.Core.Registration.Persistence.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Registration.Persistence.FileScheme.Directories qualified as Directory
import WBPS.Core.Registration.Persistence.SnarkJs.OverFileSchemeAndShh (getGenerateProvingKeyProcess, getGenerateVerificationKeyProcess)
import WBPS.Core.Registration.Registered (Registered (Registered, registrationId))
import WBPS.Core.Registration.RegistrationId (RegistrationId (RegistrationId))
import WBPS.Core.Setup.Circuit.FileScheme (
  Account (Account, registration),
  FileScheme (FileScheme, account),
  Registration (Registration, encryptionKeys),
  getPerformanceLogFilepath,
  getShellLogsFilepath,
 )

register ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey ->
  m Registered
register userWalletPublicKey = do
  let registrationId = RegistrationId userWalletPublicKey
  loadRegisteredMaybe registrationId
    >>= \case
      Just Registered {registrationId = existingRegistrationId} -> throwError [AccountAlreadyRegistered existingRegistrationId]
      Nothing -> do
        register' registrationId =<< deriveAccountDirectoryFrom registrationId
        loadExistingRegistered registrationId

register' ::
  (MonadIO m, MonadReader FileScheme m) =>
  RegistrationId ->
  Directory.Account ->
  m ()
register' registrationId accountDirectory = do
  FileScheme {account = Account {registration = Registration {encryptionKeys}}} <- ask
  ensureDir accountDirectory
  ElGamal.generateKeyPair >>= writeTo (accountDirectory </> [reldir|registered|] </> encryptionKeys)
  generateProvingKeyProcess <- getGenerateProvingKeyProcess accountDirectory
  generateVerificationKeyProcess <- getGenerateVerificationKeyProcess accountDirectory
  shellLogsFilepath <- getShellLogsFilepath accountDirectory
  perfLogPath <- getPerformanceLogFilepath
  let tags = Map.fromList [(Text.pack "registrationId", Text.pack (show registrationId))]
  liftIO $
    withPerfEventIO
      perfLogPath
      (Text.pack "snarkjs.proving.key")
      tags
      ( generateProvingKeyProcess
          &!> StdOut
          &> Append shellLogsFilepath
      )
      >> withPerfEventIO
        perfLogPath
        (Text.pack "snarkjs.public.verification.context")
        tags
        ( generateVerificationKeyProcess
            &!> StdOut
            &> Append shellLogsFilepath
        )
