module WBPS.Core.Failure (
  RegistrationFailed (..),
  -- | Different failures that can occur during the registration process
  toWBPSFailure,
  -- | Convert an Either String a to a MonadError with RegistrationFailed
) where

import Control.Monad.Except (MonadError (throwError))
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Account (AccountId)

toWBPSFailure :: MonadError [RegistrationFailed] m => Either String a -> m a
toWBPSFailure = either (throwError . pure . BuildCommitmentFailed) pure

data RegistrationFailed
  = AccountIdInvalidToCreateAFolder AccountId
  | VerificationNotFound UserWalletPublicKey
  | EncryptionKeysNotFound UserWalletPublicKey
  | AccountAlreadyRegistered UserWalletPublicKey
  | AccountNotFound UserWalletPublicKey
  | BuildCommitmentFailed String
  deriving (Show, Eq)
