-- | Compute the Schnorr challenge that the circuit labels P3.
--   This mirrors RebuildChallenge in `wbps/circuits/wbps_cardano.circom`:
--     c = SHA-512(R || X || mu_bits)
--   where R and X are Ed25519 public keys (per-byte bit-reversed in-circuit)
--   and mu_bits are the fixed-length message bits (LSB-first per byte).
module WBPS.Core.Session.Proving.Challenge (
  Challenge (..),
  compute,
  computeByUsingTxId,
  toWord8s,
) where

import Cardano.Api qualified as Api
import Crypto.Hash (Digest, SHA512, digestFromByteString, hash)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withArray)
import Data.Aeson.Types (Parser)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Data.Word (Word8)
import WBPS.Adapter.CardanoCryptoClass.Crypto qualified as Crypto
import WBPS.Core.Cardano.UnsignedTx (UnsignedTx (txUnsigned))
import WBPS.Core.Keys.Ed25519 (PublicKey (PublicKey), UserWalletPublicKey (UserWalletPublicKey))
import WBPS.Core.Session.Demonstration.PreparedMessage (
  Message (Message),
  MessageBits (MessageBits),
 )
import WBPS.Core.Session.Demonstration.R (R (R))

-- | Challenge digest used by the transcript.
newtype Challenge = Challenge (Digest SHA512)
  deriving (Eq, Show)

instance ToJSON Challenge where
  toJSON (Challenge digest) =
    toJSON (BS.unpack (BA.convert digest :: ByteString))

instance FromJSON Challenge where
  parseJSON = withArray "Challenge" $ \arr -> do
    bytes <- traverse parseJSON (V.toList arr) :: Parser [Word8]
    case digestFromByteString (BS.pack bytes) of
      Nothing -> fail "Challenge: invalid digest length"
      Just digest -> pure (Challenge digest)

-- | Recompute the transcript challenge as done in the circuit.
--   The message is first expanded to the fixed circuit length via 'toBitsPaddedToMaxSize',
--   then its bytes are bit-reversed to match the circuit's LSB-first bit ordering.
compute :: UserWalletPublicKey -> MessageBits -> R -> Challenge
compute (UserWalletPublicKey userPk) (MessageBits messageBits) (R rPk) =
  Challenge (hash (publicKeyBytes rPk <> publicKeyBytes userPk <> messageBits))

-- | Recompute the transcript challenge using the transaction id instead of the
--   full message bits. The tx id bytes are bit-reversed per byte to match the
--   circuit's LSB-first bit ordering.
computeByUsingTxId :: UserWalletPublicKey -> Message -> R -> Challenge
computeByUsingTxId (UserWalletPublicKey userPk) message (R rPk) =
  Challenge (hash (rBytes <> xBytes <> txIdBytesReversed))
  where
    rBytes = publicKeyBytes rPk
    xBytes = publicKeyBytes userPk
    txIdBytes = txIdFromMessage message
    txIdBytesReversed = BS.map reverseBits8 txIdBytes

toWord8s :: Challenge -> [Word8]
toWord8s (Challenge digest) =
  BS.unpack (BA.convert digest :: ByteString)

publicKeyBytes :: PublicKey -> ByteString
publicKeyBytes (PublicKey pk) =
  Crypto.toByteString pk

txIdFromMessage :: Message -> ByteString
txIdFromMessage (Message unsignedTx) =
  Api.serialiseToRawBytes (Api.getTxId (txUnsigned unsignedTx))

reverseBits8 :: Word8 -> Word8
reverseBits8 b =
  ((b .&. 0x01) `shiftL` 7)
    .|. ((b .&. 0x02) `shiftL` 5)
    .|. ((b .&. 0x04) `shiftL` 3)
    .|. ((b .&. 0x08) `shiftL` 1)
    .|. ((b .&. 0x10) `shiftR` 1)
    .|. ((b .&. 0x20) `shiftR` 3)
    .|. ((b .&. 0x40) `shiftR` 5)
    .|. ((b .&. 0x80) `shiftR` 7)
