{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module WBPS.Core.Session.Steps.Demonstration.Demonstrate (
  demonstrate,
) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Default (Default (def))
import WBPS.Core.Failure (WBPSFailure)
import WBPS.Core.Registration.Artefacts.Groth16.Setup (Setup (Setup, encryptionKeys))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Artefacts.Keys.ElGamal (EncryptionKey)
import WBPS.Core.Registration.Artefacts.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Registration.FetchAccounts (loadRegistered)
import WBPS.Core.Registration.Registered (Registered (Registered, setup))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment.Build (Input (Input), build)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (CircuitMessage (message), circuit)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage.Prepare (prepare)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Rho qualified as Rho
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Scalars as Scalars (
  Scalars (Scalars, ekPowRho),
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Scalars.Compute qualified as Scalars
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (
  CommitmentDemonstrated (
    CommitmentDemonstrated,
    commitment,
    preparedMessage,
    scalars
  ),
 )
import WBPS.Core.Session.Steps.Demonstration.Persistence.Events (EventHistory (EventHistory), persist)
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)

demonstrate ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> UnsignedTx -> m EventHistory
demonstrate userWalletPublicKey unsignedTx = do
  (registered, ek) <- project userWalletPublicKey
  preparedMessage <- prepare def unsignedTx
  scalars@Scalars {ekPowRho} <- Scalars.compute ek =<< Rho.generateElGamalExponent
  commitment <- build userWalletPublicKey . Input ekPowRho . message . circuit $ preparedMessage
  EventHistory registered
    <$> persist
      registered
      CommitmentDemonstrated
        { preparedMessage
        , scalars
        , commitment
        }

project :: (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) => UserWalletPublicKey -> m (Registered, EncryptionKey)
project userWalletPublicKey = do
  registered@Registered {setup = Setup {encryptionKeys = ElGamal.KeyPair {ek}}} <- loadRegistered userWalletPublicKey
  return (registered, ek)
