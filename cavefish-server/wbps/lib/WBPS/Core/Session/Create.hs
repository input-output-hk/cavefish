{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module WBPS.Core.Session.Create (
  create,
  Session (..),
) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WBPS.Core.Cardano.UnsignedTx (UnsignedTx, randomizeTx, toAbstractUnsignedTx)
import WBPS.Core.Failure (
  RegistrationFailed (AccountNotFound),
 )
import WBPS.Core.FileScheme (FileScheme)
import WBPS.Core.Groth16.Setup (Setup (Setup, encryptionKeys))
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Keys.ElGamal (
  Rho,
 )
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Registration.Account (AccountCreated (AccountCreated, setup))
import WBPS.Core.Registration.FetchAccounts (loadAccount)
import WBPS.Core.Session.Commitment.BuildCommitment (
  Commitment,
 )
import WBPS.Core.Session.Commitment.Commitment (Message (Message), PublicMessage (PublicMessage), builCommitment)
import WBPS.Core.Session.Commitment.Scalars as CommitmentScalars (
  CommitmentScalars (CommitmentScalars, ekPowRho),
  compute,
 )

data Session
  = SessionCreated
  { userWalletPublicKey :: UserWalletPublicKey
  , setup :: Setup
  , message :: Message
  , rho :: Rho
  , commitmentScalars :: CommitmentScalars
  , commitment :: Commitment
  , publicMessage :: PublicMessage
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

create ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> UnsignedTx -> m Session
create userWalletPublicKey unsignedTx =
  loadAccount userWalletPublicKey
    >>= \case
      Nothing -> throwError [AccountNotFound userWalletPublicKey]
      Just AccountCreated {setup = setup@Setup {encryptionKeys = ElGamal.KeyPair {ek}}} -> do
        rho <- ElGamal.generateElGamalExponent
        commitmentScalars@CommitmentScalars {ekPowRho} <- CommitmentScalars.compute ek rho
        message <- Message <$> randomizeTx unsignedTx
        commitment <- builCommitment ekPowRho message
        return
          SessionCreated
            { publicMessage = PublicMessage . toAbstractUnsignedTx $ unsignedTx
            , ..
            }
