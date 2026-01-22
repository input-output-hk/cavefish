module WBPS.Core.Session.Steps.Submitting.Artefacts.SubmittedTx (
  SubmittedTx (..),
) where

import Cardano.Api (FromJSON, ToJSON)
import Cardano.Api qualified as Api
import Data.Aeson qualified as Aeson

newtype SubmittedTx = SubmittedTx {unSubmittedTx :: Api.Tx Api.ConwayEra}
  deriving newtype (Eq, Show)

instance ToJSON SubmittedTx where
  toJSON (SubmittedTx tx) =
    Aeson.toJSON (Api.serialiseToTextEnvelope Nothing tx)

instance FromJSON SubmittedTx where
  parseJSON v = do
    envelope <- Aeson.parseJSON v
    case Api.deserialiseFromTextEnvelope @(Api.Tx Api.ConwayEra) envelope of
      Left err -> fail (show err)
      Right tx -> pure (SubmittedTx tx)
