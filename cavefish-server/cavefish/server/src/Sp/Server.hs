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
import Cavefish.Endpoints.Write.Demonstrate qualified as Demonstrate
import Cavefish.Endpoints.Write.Prove qualified as Prove
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
    :<|> Demonstrate
    :<|> Prove
    -- :<|> "askSubmission" :> ReqBody '[JSON] AskSubmission.Inputs :> Post '[JSON] AskSubmission.Outputs
    :<|> FetchAccount
    :<|> FetchAccounts

type Register = "register" :> ReqBody '[JSON] Register.Inputs :> Post '[JSON] Register.Outputs

type Demonstrate =
  "demonstrate"
    :> ReqBody '[JSON] Demonstrate.Inputs
    :> Post '[JSON] Demonstrate.Outputs

type Prove =
  "prove"
    :> ReqBody '[JSON] Prove.Inputs
    :> Post '[JSON] Prove.Outputs

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
    :<|> Demonstrate.handle
    :<|> Prove.handle
    -- :<|> AskSubmission.handle
    :<|> FetchAccount.handle
    :<|> FetchAccounts.handle
