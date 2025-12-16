module Adapter.Cavefish.Client (
  setupCavefish,
  Setup (..),
  ServiceProviderAPI (..),
  WriteAPI (..),
  ReadAPI (..),
) where

import Cardano.Api qualified as C
import Cavefish.Api.ServerConfiguration (
  ServerConfiguration (..),
 )
import Cavefish.Endpoints.Read.FetchAccount qualified as FetchAccount
import Cavefish.Endpoints.Read.FetchAccounts qualified as FetchAccounts
import Cavefish.Endpoints.Write.DemonstrateCommitment qualified as DemonstrateCommitment
import Cavefish.Endpoints.Write.Register qualified as Register
import Cavefish.Services.TxBuilding (ServiceFee (..))
import Control.Monad ((>=>))
import Cooked hiding (Wallet, distributionFromList)
import Data.Coerce (coerce)
import Data.Default (Default (def))
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Ledger qualified
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp qualified as Warp
import Plutus.Script.Utils.Value (ada)
import PlutusLedgerApi.V3 qualified as Api
import Prototype.AskCommitmentProof qualified as AskCommitmentProof
import Servant (Application, Proxy (Proxy), type (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (BaseUrl))
import Servant.Client qualified as SC
import Sp.Emulator (mkServerContext)
import Sp.Middleware (errStatusTraceMiddleware)
import Sp.Server (Cavefish, mkServer)
import Test.Hspec (expectationFailure)
import WBPS.Core.FileScheme (FileScheme, mkFileSchemeFromRoot)
import WBPS.Core.Keys.Ed25519 (PaymentAddess (PaymentAddess), Wallet (..), generateWallet)

getServiceProviderAPI :: ServiceFee -> Int -> IO ServiceProviderAPI
getServiceProviderAPI fee port = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl SC.Http "127.0.0.1" port ""
      ( register
          :<|> demonstrateCommitment
          :<|> askCommitmentProof
          :<|> _
          :<|> fetchAccount
          :<|> fetchAccounts
          :<|> _
          :<|> _
        ) = SC.client (Proxy @Cavefish)
  return
    ServiceProviderAPI
      { fee = fee
      , write =
          WriteAPI
            { register = runClientOrFail (SC.mkClientEnv manager baseUrl) . register
            , demonstrateCommitment = runClientOrFail (SC.mkClientEnv manager baseUrl) . demonstrateCommitment
            , askCommitmentProof = runClientOrFail (SC.mkClientEnv manager baseUrl) . askCommitmentProof
            }
      , read =
          ReadAPI
            { fetchAccount = runClientOrFail (SC.mkClientEnv manager baseUrl) . fetchAccount
            , fetchAccounts = runClientOrFail (SC.mkClientEnv manager baseUrl) fetchAccounts
            }
      }

data ServiceProviderAPI
  = ServiceProviderAPI
  { fee :: ServiceFee
  , write :: WriteAPI
  , read :: ReadAPI
  }

data WriteAPI = WriteAPI
  { register :: Register.Inputs -> IO Register.Outputs
  , demonstrateCommitment :: DemonstrateCommitment.Inputs -> IO DemonstrateCommitment.Outputs
  , askCommitmentProof :: AskCommitmentProof.Inputs -> IO AskCommitmentProof.Outputs
  }

data ReadAPI = ReadAPI
  { fetchAccount :: FetchAccount.Inputs -> IO FetchAccount.Outputs
  , fetchAccounts :: IO FetchAccounts.Outputs
  }

runClientOrFail :: SC.ClientEnv -> SC.ClientM a -> IO a
runClientOrFail clientEnv action = do
  SC.runClientM action clientEnv
    >>= \case
      Left err -> expectationFailure ("HTTP client call failed: " <> show err) >> fail "http client failure"
      Right value -> pure value

mkTestCavefishMonad ::
  FileScheme ->
  Cooked.InitialDistribution ->
  ServerConfiguration ->
  Application
mkTestCavefishMonad wbpsScheme initialDistribution serverConfiguration =
  mkServer errStatusTraceMiddleware $
    mkServerContext
      initialDistribution
      wbpsScheme
      serverConfiguration

setupCavefish :: (Setup -> IO a) -> IO a
setupCavefish actions = do
  wbpsScheme <- mkFileSchemeFromRoot "../../wbps"
  alice <- generateWallet
  bob <- generateWallet
  provider@Wallet {paymentAddress} <- generateWallet
  let servicefee = ServiceFee {amount = 10_000_000, paidTo = paymentAddress}
  Warp.testWithApplication
    ( pure $
        mkTestCavefishMonad
          wbpsScheme
          ( distributionFromList
              [ (alice, [ada 100])
              , (bob, [ada 200])
              , (provider, [ada 10])
              ]
          )
          ServerConfiguration
            { httpServer = def
            , wbps = def
            , serviceProviderFee = servicefee
            , transactionExpiry = def
            }
    )
    (getServiceProviderAPI servicefee >=> \serviceProvider -> actions Setup {..})

distributionFromList :: [(Wallet, [Api.Value])] -> Cooked.InitialDistribution
distributionFromList =
  Cooked.InitialDistribution
    . foldl' (\x (user, values) -> x <> map (Cooked.receives user . Cooked.Value) values) []

data Setup = Setup
  { serviceProvider :: ServiceProviderAPI
  , alice :: Wallet
  , bob :: Wallet
  , provider :: Wallet
  }
