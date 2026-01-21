{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Sp.Server (
  Cavefish,
  Register,
  mkServer,
) where

import Cavefish (CavefishServerM, CavefishServices, runCavefishMonad)
import Cavefish.Endpoints.Read.FetchAccount qualified as FetchAccount
import Cavefish.Endpoints.Read.FetchAccounts qualified as FetchAccounts
import Cavefish.Endpoints.Write.DemonstrateCommitment qualified as DemonstrateCommitment
import Cavefish.Endpoints.Write.ProveCommitment qualified as ProveCommitment
import Cavefish.Endpoints.Write.Register qualified as Register
import Network.Wai (Application, Middleware)
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (corsMethods, corsRequestHeaders),
  cors,
  simpleCorsResourcePolicy,
 )
import Servant (
  Get,
  HasServer (ServerT),
  JSON,
  Post,
  Proxy (Proxy),
  ReqBody,
  hoistServer,
  serve,
 )
import Servant.API ((:<|>) ((:<|>)), (:>))

type Cavefish =
  Register
    :<|> DemonstrateCommitment
    :<|> ProveCommitment
    -- :<|> "askSubmission" :> ReqBody '[JSON] AskSubmission.Inputs :> Post '[JSON] AskSubmission.Outputs
    :<|> FetchAccount
    :<|> FetchAccounts

type Register = "register" :> ReqBody '[JSON] Register.Inputs :> Post '[JSON] Register.Outputs

type DemonstrateCommitment =
  "demonstrateCommitment"
    :> ReqBody '[JSON] DemonstrateCommitment.Inputs
    :> Post '[JSON] DemonstrateCommitment.Outputs

type ProveCommitment =
  "proveCommitment"
    :> ReqBody '[JSON] ProveCommitment.Inputs
    :> Post '[JSON] ProveCommitment.Outputs

type FetchAccount =
  "fetchAccount" :> ReqBody '[JSON] FetchAccount.Inputs :> Post '[JSON] FetchAccount.Outputs

type FetchAccounts = "fetchAccounts" :> Get '[JSON] FetchAccounts.Outputs

cavefishApi :: Proxy Cavefish
cavefishApi = Proxy

mkServer :: Middleware -> CavefishServices -> Application
mkServer cavefishMiddleware env =
  let
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        , corsMethods = ["GET", "POST", "OPTIONS"]
        }
   in
    cors (const $ Just policy) $
      cavefishMiddleware $
        serve cavefishApi $
          hoistServer cavefishApi (runCavefishMonad env) server

server :: ServerT Cavefish CavefishServerM
server =
  Register.handle
    :<|> DemonstrateCommitment.handle
    :<|> ProveCommitment.handle
    -- :<|> AskSubmission.handle
    :<|> FetchAccount.handle
    :<|> FetchAccounts.handle
