module WBPS.Specs.Session.BlindSigning.ThetaStatement (specs) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import WBPS.Core.Session.BlindSigning.ThetaStatement (
  ThetaStatement (ThetaStatement),
  rebuildThetaStatementFromDemonstrated,
 )
import WBPS.Specs.Session.BlindSigning.ThetaStatementFixture (
  ThetaStatementFixture (ThetaStatementFixture, bigR, challenge, commitmentDemonstrated, expectedStatement, userWalletPublicKey),
  loadThetaStatementFixture,
 )

specs :: TestTree
specs =
  testGroup
    "BlindSigning"
    [ testCase "rebuilds statement.json from output artefacts" rebuildsStatement
    ]

rebuildsStatement :: Assertion
rebuildsStatement = do
  ThetaStatementFixture {userWalletPublicKey, commitmentDemonstrated, challenge, bigR, expectedStatement} <-
    loadThetaStatementFixture

  let ThetaStatement actual =
        rebuildThetaStatementFromDemonstrated userWalletPublicKey bigR challenge commitmentDemonstrated

  actual @?= expectedStatement
