{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Cavefish.Nominal (spec) where

import Adapter.Cavefish.Client (
  ReadAPI (ReadAPI, fetchAccount),
  ServiceProviderAPI (ServiceProviderAPI, read, write),
  Setup (Setup, alice, bob, serviceProvider, userToolkit),
  UserToolkitAPI (UserToolkitAPI, assertProofIsValid, signBlindly),
  WriteAPI (
    WriteAPI,
    demonstrate,
    prove,
    register
  ),
  setupCavefish,
 )
import Cardano.Api (lovelaceToValue)
import Cavefish.Endpoints.Read.FetchAccount qualified as FetchAccount
import Cavefish.Endpoints.Write.Demonstrate qualified as Demonstrate
import Cavefish.Endpoints.Write.Prove qualified as Prove
import Cavefish.Endpoints.Write.Register qualified as Register
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NE
import Intent.Example.DSL (AddressW (AddressW), IntentDSL (AndExpsW, PayToW, SpendFromW), satisfies)
import Path (reldir)
import Test.Hspec (Spec, describe, it, shouldBe)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (
  PaymentAddess (PaymentAddess),
  keyPair,
  paymentAddress,
  publicKey,
 )
import WBPS.Core.Session.BlindSigning.ThetaStatement (rebuildThetaStatement)
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (Commitment (Commitment, id, payload))
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage (PublicMessage (PublicMessage))
import WBPS.Core.Session.Demonstration.Artefacts.R qualified as R

spec :: Spec
spec = do
  describe "[Cavefish Server - Integration Spec]" $
    describe "Nominal Cases" $ do
      it
        "register, demonstrate, prove, verify, blindly-sign and end up with signed transaction"
        $ do
          setupCavefish
            [reldir|integration-cavefish-nominal-flow|]
            \Setup
               { serviceProvider =
                 ServiceProviderAPI
                   { write = WriteAPI {register, demonstrate, prove}
                   , read = ReadAPI {fetchAccount}
                   }
               , userToolkit = UserToolkitAPI {assertProofIsValid, signBlindly}
               , alice
               , bob
               } -> do
                let intent =
                      AndExpsW
                        ( NE.fromList
                            [ SpendFromW (coerce . paymentAddress $ alice)
                            , PayToW (lovelaceToValue 10_000_000) (coerce . paymentAddress $ bob)
                            ]
                        )
                Register.Outputs {publicVerificationContext, ek} <- register . Register.Inputs . publicKey $ alice

                Demonstrate.Outputs {commitment = commitment@Commitment {id = commitmentId, payload}, txAbs} <-
                  demonstrate
                    . Demonstrate.Inputs (publicKey alice)
                    $ intent

                satisfies intent txAbs `shouldBe` True

                (r, bigR) <- R.generateKeyTuple

                Prove.Outputs {challenge, proof} <-
                  prove
                    Prove.Inputs
                      { userWalletPublicKey = publicKey alice
                      , commitmentId
                      , bigR = bigR
                      }

                assertProofIsValid
                  publicVerificationContext
                  (rebuildThetaStatement (publicKey alice) bigR challenge payload (PublicMessage txAbs))
                  proof

                signature <- signBlindly (keyPair alice) r challenge

                FetchAccount.Outputs {accountMaybe} <- fetchAccount . FetchAccount.Inputs . publicKey $ alice

                accountMaybe
                  `shouldBe` Just FetchAccount.Account {userWalletPublicKey = publicKey alice, ek, publicVerificationContext}
