{-# LANGUAGE OverloadedStrings #-}

module Core.IntentDSLGen (
  genDSL,
) where

import Cardano.Api qualified as Api
import Cooked qualified
import Data.List.NonEmpty (NonEmpty ((:|)))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Intent.Example.DSL (
  AddressW (AddressW),
  IntentDSL (AndExpsW, ChangeToW, MaxFeeW, PayToW, SpendFromW),
 )
import Ledger.CardanoWallet qualified as Ledger
import Test.Gen.Cardano.Api.Typed (
  genQuantity,
  genValue,
 )

genPayToW :: AddressW -> Gen IntentDSL
genPayToW addressW = do
  value <- genValue (pure $ Api.AdaAssetId) (genQuantity $ Range.constant 13 17)
  pure $ PayToW value addressW

genDSL :: Gen IntentDSL
genDSL = do
  {- TODO Minting is not supported yet. Jan/22/26, -KK
    mustMintW <- MustMintW <$> genValue genAssetId (genQuantity $ Range.linear 1 10)
  -}
  let
    alice = AddressW . Api.serialiseAddress $ Ledger.mockWalletAddress $ Cooked.wallet 1
    mary = AddressW . Api.serialiseAddress $ Ledger.mockWalletAddress $ Cooked.wallet 2
    bob = AddressW . Api.serialiseAddress $ Ledger.mockWalletAddress $ Cooked.wallet 3
    spendFromW = SpendFromW alice
    changeToW = ChangeToW mary
  payToW <- genPayToW bob
  maxFeeW <- MaxFeeW <$> Gen.integral (Range.constant 10 15)
  pure $ AndExpsW $ spendFromW :| [payToW, changeToW, maxFeeW]
