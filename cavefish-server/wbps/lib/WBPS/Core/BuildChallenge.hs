module WBPS.Core.BuildChallenge where

import Crypto.Hash (Digest, SHA512, hash)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

-- | Compute the challenge digest from R, X, and a TxId (already Blake2b-256(tx body)).
-- We follow the high-level spec:
--
--   input = encodePointEdDSA(R) || encodePointEdDSA(X) || encodeDigest(txId)
--   c     = SHA512(input)
--
-- R and X are passed as their EdDSA-encoded bytes; txId bytes are taken directly
-- from the Cardano API.
buildChallenge ::
  -- | R bytes
  ByteString ->
  -- | X bytes
  ByteString ->
  -- | txId bytes
  ByteString ->
  Either String (Digest SHA512)
buildChallenge rBytes xBytes txIdBytes
  | BS.length rBytes /= 32 = Left "expected 32-byte commitment point"
  | BS.length xBytes /= 32 = Left "expected 32-byte signer key"
  | otherwise =
      Right (hash (BS.concat [rBytes, xBytes, txIdBytes]))

-- pragma circom 2.1.2;

-- include "hashing/sha2/sha512/sha512_hash_bits.circom";

-- // ======================================================================
-- // 4) RebuildChallenge — P3
-- // ----------------------------------------------------------------------
-- // Inputs:
-- //   in_commitment_point[256] : R bits (nonce commitment g^ρ), byte-wise bit-reversed per EdDSA
-- //   in_signer_key[256]       : X bits (signer key), byte-wise bit-reversed per EdDSA
-- //   in_message[message_size]   : μ_bits
-- // Output:
-- //   out_challenge[64]        : digest
-- //
-- // Property (P3):
-- //   RebuildChallenge(R, X, μ) → digest and assert challenge == digest
-- //   (digest = SHA512(R || X || μ_bits) with per-byte bit-reversal for R, X)
-- // ======================================================================
-- template RebuildChallenge(message_size) {
--     signal input  in_commitment_point[256];
--     signal input  in_signer_key[256];
--     signal input  in_message[message_size];
--     signal output out_challenge[64];

--     component hash = Sha512_hash_bits_digest(message_size + 512);

--     var i;
--     var j;

--     for (i = 0; i < 256; i += 8) {
--         for (j = 0; j < 8; j++) {
--             hash.inp_bits[i + j]       <== in_commitment_point[i + (7 - j)];
--             hash.inp_bits[256 + i + j] <== in_signer_key[i + (7 - j)];
--         }
--     }
--     for (i = 0; i < message_size; i++) {
--         in_message[i] ==> hash.inp_bits[512 + i];
--     }

--     for (i = 0; i < 64; i++) {
--         out_challenge[i] <== hash.hash_bytes[i];
--         log(900300 + i);
--         log(out_challenge[i]);
--     }
-- }

-- component main = BuildChallenge(131544);
