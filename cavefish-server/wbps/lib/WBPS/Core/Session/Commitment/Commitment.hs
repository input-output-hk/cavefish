{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module WBPS.Core.Session.Commitment.Commitment (
  Message (..),
  PublicMessage (..),
  builCommitment,
) where

import Cardano.Api qualified as Api
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bits (testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import WBPS.Core.Cardano.UnsignedTx (AbstractUnsignedTx (AbstractUnsignedTx), UnsignedTx (UnsignedTx), txUnsigned)
import WBPS.Core.Failure (
  RegistrationFailed,
  toWBPSFailure,
 )
import WBPS.Core.FileScheme (FileScheme)
import WBPS.Core.Keys.ElGamal (
  AffinePoint,
 )
import WBPS.Core.Primitives.Circom (BuildCommitmentParams (BuildCommitmentParams), defCommitmentParams)
import WBPS.Core.Session.Commitment.BuildCommitment (
  BuildCommitmentInput (BuildCommitmentInput, ekPowRho, messageBits),
  BuildCommitmentOutput (BuildCommitmentOutput, maskedChunks),
  Commitment,
  CommitmentPayload (CommitmentPayload),
  mkCommitment,
  runBuildCommitment,
 )

newtype Message = Message UnsignedTx
  deriving newtype (Eq, Show, FromJSON, ToJSON)

newtype PublicMessage = PublicMessage AbstractUnsignedTx
  deriving newtype (Eq, Show, FromJSON, ToJSON)

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

-- Convert a bytestring to a little-endian bit vector.
payloadBits :: ByteString -> [Int]
payloadBits bs = concatMap byteToBits (BS.unpack bs)
  where
    byteToBits b =
      [ if testBit b i then 1 else 0
      | i <- [0 .. 7]
      ]
