{-# LANGUAGE QuasiQuotes #-}

module WBPS.Specs.Session.Submitting.Submit (specs) where

import Control.Monad.Except (MonadError, catchError, throwError)
import Data.Bits (xor)
import Data.Word (Word8)
import Path (reldir)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)
import WBPS.Core.Failure (WBPSFailure (BlindSignatureFailed, BuildCommitmentFailed))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (generateKeyPair, userWalletPK)
import WBPS.Core.Registration.Register (register)
import WBPS.Core.Session.Session (Session (Demonstrated))
import WBPS.Core.Session.Steps.BlindSigning.Sign (BlindSignature (BlindSignature), sign)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (Commitment (Commitment), CommitmentId)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (generateKeyTuple)
import WBPS.Core.Session.Steps.Demonstration.Demonstrate (demonstrate)
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated))
import WBPS.Core.Session.Steps.Proving.Prove (prove)
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved (CommitmentProved, challenge))
import WBPS.Core.Session.Steps.Submitting.Submit (submit)
import WBPS.Core.Setup.Circuit.FileScheme (defaultFileScheme)
import WBPS.Specs.Adapter.Fixture (CommitmentFixtures (unsignedTxFixture), commitmentFixtures, readFixture)
import WBPS.Specs.Adapter.Test (getRootFolder)
import WBPS.WBPS (runWBPS)

specs :: TestTree
specs =
  testGroup
    "Submitting"
    [ testCase "submit validates blind signature" submitValidatesSignature
    ]

submitValidatesSignature :: Assertion
submitValidatesSignature = do
  rootFolders <- getRootFolder [reldir|integration-wbps-submit-flow|]
  let fileScheme = defaultFileScheme rootFolders
  unsignedTx <- readFixture . unsignedTxFixture . commitmentFixtures $ rootFolders
  keyPair <- generateKeyPair
  (nonceSecret, noncePublic) <- generateKeyTuple

  result <-
    runWBPS fileScheme $ do
      let userWalletPublicKey = userWalletPK keyPair
      _ <- register userWalletPublicKey
      session <- demonstrate userWalletPublicKey unsignedTx
      commitmentId <- commitmentIdFromSession session
      CommitmentProved {challenge} <- prove userWalletPublicKey commitmentId noncePublic
      signature <- sign keyPair nonceSecret challenge
      _ <- submit userWalletPublicKey commitmentId signature
      let badSignature = tamperSignature signature
      (submit userWalletPublicKey commitmentId badSignature >> pure Nothing)
        `catchError` (pure . Just)

  case result of
    Left failures ->
      assertFailure ("submit flow failed: " <> show failures)
    Right Nothing ->
      assertFailure "expected submit to reject a tampered signature"
    Right (Just failures) ->
      assertBool "expected BlindSignatureFailed" (any isBlindSignatureFailed failures)

tamperSignature :: BlindSignature -> BlindSignature
tamperSignature (BlindSignature bytes) =
  case bytes of
    [] -> BlindSignature [1]
    (b : rest) -> BlindSignature (flipBit b : rest)
  where
    flipBit :: Word8 -> Word8
    flipBit byte = byte `xor` 1

isBlindSignatureFailed :: WBPSFailure -> Bool
isBlindSignatureFailed failure =
  case failure of
    BlindSignatureFailed _ -> True
    _ -> False

commitmentIdFromSession ::
  MonadError [WBPSFailure] m =>
  Session ->
  m CommitmentId
commitmentIdFromSession session =
  case session of
    Demonstrated _ (CommitmentDemonstrated _ _ (Commitment commitmentId _)) ->
      pure commitmentId
    _ ->
      throwError [BuildCommitmentFailed "Expected a demonstrated session."]
