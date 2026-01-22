module WBPS.Core.Failure (
  WBPSFailure (..),
  -- | Different failures that can occur during the registration process
  toWBPSFailure,
  -- | Convert an Either String a to a MonadError with WBPSFailure
) where

import Control.Monad.Except (MonadError (throwError))

-- Keep lightweight aliases here to avoid cyclic imports between Failure and
-- session/registration modules that define the real types.
type AccountId = String

type SessionId = String

type CommitmentId = String

type UserWalletPublicKey = String

toWBPSFailure :: MonadError [WBPSFailure] m => Either String a -> m a
toWBPSFailure = either (throwError . pure . BuildCommitmentFailed) pure

data WBPSFailure
  = -- Account/registration failures
    AccountIdInvalidToCreateAFolder AccountId
  | VerificationNotFound UserWalletPublicKey
  | EncryptionKeysNotFound UserWalletPublicKey
  | AccountAlreadyRegistered UserWalletPublicKey
  | AccountNotFound UserWalletPublicKey
  | -- Commitment/circuit failures
    BuildCommitmentFailed String
  | CircuitMessageDecodeFailed String
  | -- Transaction assembly failures
    TxBuiltTooLarge String
  | TxInputsCountMismatch String
  | -- Session directory/session lookup failures
    SessionIdInvalidToCreateAFolder SessionId
  | SessionNotFound UserWalletPublicKey CommitmentId
  | -- Session artefact persistence failures
    SessionMessageNotFound UserWalletPublicKey CommitmentId
  | SessionRhoNotFound UserWalletPublicKey CommitmentId
  | SessionScalarsNotFound UserWalletPublicKey CommitmentId
  | SessionCommitmentNotFound UserWalletPublicKey CommitmentId
  | SessionProofNotFound UserWalletPublicKey CommitmentId
  | BlindSignatureNotFound UserWalletPublicKey CommitmentId
  | TxSignatureNotFound UserWalletPublicKey CommitmentId
  | SubmittedTxNotFound UserWalletPublicKey CommitmentId
  | -- Proof/signature failures
    ProofVerificationFailed String
  | BlindSignatureFailed String
  deriving (Show, Eq)
