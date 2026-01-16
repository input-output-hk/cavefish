{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.Session.BlindSigning.VerifyProof (
  verifyProof,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Char (toLower)
import Path (relfile, toFilePath, (</>))
import Path.IO (withSystemTempDir)
import Shh (captureTrim, (|>))
import WBPS.Adapter.Path (writeTo)
import WBPS.Core.Primitives.Snarkjs qualified as Snarkjs
import WBPS.Core.Registration.Artefacts.Groth16.Setup (
  PublicVerificationContext (PublicVerificationContext, asJson),
 )
import WBPS.Core.Session.BlindSigning.ThetaStatement (ThetaStatement)
import WBPS.Core.Session.Proving.Artefacts.Proof (Proof)

verifyProof ::
  MonadIO m =>
  PublicVerificationContext ->
  ThetaStatement ->
  Proof ->
  m Bool
verifyProof PublicVerificationContext {asJson} statement proof =
  liftIO $
    withSystemTempDir "wbps-verify-proof-" $ \tmpDir -> do
      let verificationKeyPath = tmpDir </> [relfile|verification_key.json|]
          statementPath = tmpDir </> [relfile|statement.json|]
          proofPath = tmpDir </> [relfile|proof.json|]
      writeTo verificationKeyPath asJson
      writeTo statementPath statement
      writeTo proofPath proof
      output <-
        Snarkjs.verify
          Snarkjs.VerifyScheme
            { verificationKey = toFilePath verificationKeyPath
            , statement = toFilePath statementPath
            , proof = toFilePath proofPath
            }
          |> captureTrim
      pure (isVerificationOk output)

isVerificationOk :: BL8.ByteString -> Bool
isVerificationOk output =
  let tokens = words (map toLower (BL8.unpack output))
      validTokens = ["ok", "true", "valid"]
   in notElem "invalid" tokens && any (`elem` validTokens) tokens
