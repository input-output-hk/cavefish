{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module WBPS.Commitment (
  createSession,
  Session (..),
  Message (..),
  PublicMessage (..),
  Commitment (..),
) where

import Cardano.Api qualified as Api
import Cardano.Ledger.Api qualified as Ledger
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity (Identity), runIdentity)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bits (testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import WBPS.Core.BuildCommitment (
  BuildCommitmentInput (BuildCommitmentInput, ekPowRho, messageBits),
  BuildCommitmentOutput (BuildCommitmentOutput, maskedChunks),
  Commitment (Commitment, id, payload),
  CommitmentPayload (CommitmentPayload),
  mkCommitment,
  runBuildCommitment,
 )
import WBPS.Core.Cardano.UnsignedTx (AbstractUnsignedTx (AbstractUnsignedTx), UnsignedTx (UnsignedTx), txUnsigned)
import WBPS.Core.FileScheme (FileScheme)
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Keys.ElGamal (
  AffinePoint,
  EncryptionKey,
  Rho,
  encryptionKeyPowRho,
  generatorPowRho,
 )
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Primitives.Circom (BuildCommitmentParams (BuildCommitmentParams), defCommitmentParams)
import WBPS.Registration (
  AccountCreated (AccountCreated, encryptionKeys),
  RegistrationFailed (AccountNotFound, BuildCommitmentFailed),
  loadAccount,
 )

newtype Message = Message UnsignedTx
  deriving newtype (Eq, Show, FromJSON, ToJSON)

newtype PublicMessage = PublicMessage AbstractUnsignedTx
  deriving newtype (Eq, Show, FromJSON, ToJSON)

data Session
  = SessionCreated
  { userWalletPublicKey :: UserWalletPublicKey
  , message :: Message
  , rho :: Rho
  , commitmentScalars :: CommitmentScalars
  , commitment :: Commitment
  , publicMessage :: PublicMessage
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

createSession ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> UnsignedTx -> m Session
createSession userWalletPublicKey unsignedTx =
  loadAccount userWalletPublicKey
    >>= \case
      Nothing -> throwError [AccountNotFound userWalletPublicKey]
      Just AccountCreated {encryptionKeys = ElGamal.KeyPair {ek}} -> do
        rho <- ElGamal.generateElGamalExponent
        commitmentScalars@CommitmentScalars {ekPowRho} <- computeCommitmentScalars ek rho
        message <- randomizeTx unsignedTx
        commitment <- builCommitment ekPowRho message
        return
          SessionCreated
            { publicMessage = PublicMessage . txToTxAbs $ unsignedTx
            , ..
            }

txToTxAbs :: UnsignedTx -> AbstractUnsignedTx
txToTxAbs (UnsignedTx (Api.ShelleyTxBody era body scripts scriptData metadata validity)) =
  AbstractUnsignedTx . UnsignedTx $
    Api.ShelleyTxBody era (setInputs mempty body) scripts scriptData metadata validity
  where
    setInputs ins = runIdentity . Ledger.inputsTxBodyL (\_ -> Identity ins)

-- should add aux in metadata
randomizeTx :: MonadIO m => UnsignedTx -> m Message
randomizeTx = pure . Message

builCommitment ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  AffinePoint ->
  Message ->
  m Commitment
builCommitment ekPowRho (Message message) = do
  let commitmentParams@(BuildCommitmentParams msgSize _ _) = defCommitmentParams
      messageBits = take msgSize (payloadBits (Api.serialiseToCBOR (txUnsigned message)) ++ repeat 0)

  BuildCommitmentOutput {maskedChunks} <-
    toWBPSFailure =<< runBuildCommitment commitmentParams BuildCommitmentInput {ekPowRho, messageBits}
  return $ mkCommitment (CommitmentPayload maskedChunks)

computeCommitmentScalars ::
  MonadError [RegistrationFailed] m => EncryptionKey -> Rho -> m CommitmentScalars
computeCommitmentScalars ek rho =
  CommitmentScalars
    <$> toWBPSFailure (encryptionKeyPowRho ek rho)
    <*> toWBPSFailure (generatorPowRho rho)

toWBPSFailure :: MonadError [RegistrationFailed] m => Either String a -> m a
toWBPSFailure = either (throwError . pure . BuildCommitmentFailed) pure

data CommitmentScalars
  = CommitmentScalars
  { ekPowRho :: AffinePoint
  , gPowRho :: AffinePoint
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Convert a bytestring to a little-endian bit vector.
payloadBits :: ByteString -> [Int]
payloadBits bs = concatMap byteToBits (BS.unpack bs)
  where
    byteToBits b =
      [ if testBit b i then 1 else 0
      | i <- [0 .. 7]
      ]
