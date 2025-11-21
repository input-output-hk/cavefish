{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Sp.Server where

import Core.Api.AppContext (AppM, Env, runApp)
import Core.Api.Messages (
  ClientsResp,
  CommitReq,
  CommitResp,
  FinaliseReq,
  FinaliseResp,
  PendingResp,
  TransactionResp,
  clientsH,
  commitH,
  finaliseH,
  pendingH,
  transactionH,
 )
import Core.SP.DemonstrateCommitment qualified as DemonstrateCommitment
import Core.SP.Register qualified as Register
import Data.Text (Text)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (corsMethods, corsRequestHeaders),
  cors,
  simpleCorsResourcePolicy,
 )
import Servant (
  Capture,
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

type CavefishApi =
  "register" :> ReqBody '[JSON] Register.Inputs :> Post '[JSON] Register.Outputs
    :<|> "demonstrateCommitment"
      :> ReqBody '[JSON] DemonstrateCommitment.Inputs
      :> Post '[JSON] DemonstrateCommitment.Outputs
    :<|> "commit" :> ReqBody '[JSON] CommitReq :> Post '[JSON] CommitResp
    :<|> "finalise" :> ReqBody '[JSON] FinaliseReq :> Post '[JSON] FinaliseResp
    :<|> "clients" :> Get '[JSON] ClientsResp
    :<|> "pending" :> Get '[JSON] PendingResp
    :<|> "transaction" :> Capture "id" Text :> Get '[JSON] TransactionResp

cavefishApi :: Proxy CavefishApi
cavefishApi = Proxy

mkApp :: Env -> Application
mkApp env =
  let policy =
        simpleCorsResourcePolicy
          { corsRequestHeaders = ["Content-Type"]
          , corsMethods = ["GET", "POST", "OPTIONS"]
          }
   in cors (const $ Just policy) $
        serve cavefishApi (hoistServer cavefishApi (runApp env) server)

server :: ServerT CavefishApi AppM
server =
  Register.handle
    :<|> DemonstrateCommitment.handle
    :<|> commitH
    :<|> finaliseH
    :<|> clientsH
    :<|> pendingH
    :<|> transactionH
