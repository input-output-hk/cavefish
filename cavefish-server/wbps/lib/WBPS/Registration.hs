{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module WBPS.Registration (
  register,
  withFileSchemeIO,
  loadAccount,
  loadAccounts,
  AccountCreated (..),
  RegistrationFailed (..),
  PublicVerificationContext (..),
) where

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, runReaderT)
import Data.Aeson (Value)
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import Path (Abs, Dir, File, Path, parseRelDir, toFilePath, (</>))
import Path.IO (doesDirExist, ensureDir, listDirRel)
import Shh (Stream (Append, StdOut), (&!>), (&>))
import WBPS.Adapter.Monad.Control (ifM, whenNothingThrow)
import WBPS.Adapter.Path (readFrom, writeTo)
import WBPS.Core.FileScheme (
  Account,
  AccountName,
  FileScheme (FileScheme, accounts, encryptionKeys, provingKey, verificationContext),
  getShellLogsFilepath,
 )
import WBPS.Core.Keys.Ed25519 (PublicKey (PublicKey), UserWalletPublicKey (UserWalletPublicKey))
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Primitives.SnarkjsOverFileScheme (
  getGenerateProvingKeyProcess,
  getGenerateVerificationKeyProcess,
 )

data PublicVerificationContext = PublicVerificationContext {filePath :: Path Abs File, asJson :: Value}
  deriving (Eq, Show, Ord, Generic)

data AccountCreated
  = AccountCreated
  { userWalletPublicKey :: UserWalletPublicKey
  , provingKey :: Path Abs File
  , encryptionKeys :: ElGamal.KeyPair
  , publicVerificationContext :: PublicVerificationContext
  }
  deriving (Ord, Eq, Show)

getRecordedUserWalletPublicKeys :: MonadIO m => Path b Dir -> m [UserWalletPublicKey]
getRecordedUserWalletPublicKeys p = do
  a <- fst <$> (liftIO . listDirRel $ p)
  return $ fromString . takeWhile (/= '/') . toFilePath <$> a

loadAccounts ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  m [AccountCreated]
loadAccounts = do
  FileScheme {..} <- ask
  recordedKeys <- getRecordedUserWalletPublicKeys accounts
  traverse loadAccount' recordedKeys

loadAccount ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> m (Maybe AccountCreated)
loadAccount userWalletPublicKey = do
  account <- deriveAccountFrom userWalletPublicKey
  ifM
    (not <$> doesDirExist account)
    (return Nothing)
    (Just <$> loadAccount' userWalletPublicKey)

loadAccount' ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> m AccountCreated
loadAccount' userWalletPublicKey = do
  account <- deriveAccountFrom userWalletPublicKey
  FileScheme {..} <- ask
  AccountCreated
    userWalletPublicKey
    (account </> provingKey)
    <$> (readFrom (account </> encryptionKeys) >>= whenNothingThrow [EncryptionKeysNotFound userWalletPublicKey])
    <*> ( PublicVerificationContext (account </> verificationContext)
            <$> (readFrom (account </> verificationContext) >>= whenNothingThrow [EncryptionKeysNotFound userWalletPublicKey])
        )

register ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey ->
  m AccountCreated
register userWalletPublicKey =
  loadAccount userWalletPublicKey
    >>= \case
      Just AccountCreated {userWalletPublicKey = existingUserWalletKey} -> throwError [AccountAlreadyRegistered existingUserWalletKey]
      Nothing -> do
        register' =<< deriveAccountFrom userWalletPublicKey
        loadAccount' userWalletPublicKey

deriveAccountFrom ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> m Account
deriveAccountFrom userWalletPublicKey = do
  FileScheme {..} <- ask
  (accounts </>) <$> (getAccountFileName . accountId $ userWalletPublicKey)

register' ::
  (MonadIO m, MonadReader FileScheme m) =>
  Account ->
  m ()
register' account = do
  FileScheme {..} <- ask
  ensureDir account
  ElGamal.generateKeyPair >>= writeTo (account </> encryptionKeys)
  generateProvingKeyProcess <- getGenerateProvingKeyProcess account
  generateVerificationKeyProcess <- getGenerateVerificationKeyProcess account
  shellLogsFilepath <- getShellLogsFilepath account
  liftIO $
    (generateProvingKeyProcess >> generateVerificationKeyProcess)
      &!> StdOut
      &> Append shellLogsFilepath

getAccountFileName :: MonadError [RegistrationFailed] m => AccountId -> m AccountName
getAccountFileName account@(AccountId x) =
  either
    (const . throwError $ [AccountIdInvalidToCreateAFolder account])
    pure
    (Path.parseRelDir x)

withFileSchemeIO ::
  FileScheme ->
  (forall m. (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) => m a) ->
  IO (Either [RegistrationFailed] a)
withFileSchemeIO scheme action =
  runExceptT $ runReaderT action scheme

newtype AccountId = AccountId String deriving (Show, Eq)

data RegistrationFailed
  = AccountIdInvalidToCreateAFolder AccountId
  | VerificationNotFound UserWalletPublicKey
  | EncryptionKeysNotFound UserWalletPublicKey
  | AccountAlreadyRegistered UserWalletPublicKey
  | AccountNotFound UserWalletPublicKey
  | BuildCommitmentFailed String
  deriving (Show, Eq)

accountId :: UserWalletPublicKey -> AccountId
accountId (UserWalletPublicKey (PublicKey x)) = AccountId . show $ x
