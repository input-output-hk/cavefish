module WBPS.Core.Cardano.UnsignedTx (
  UnsignedTx (..),
  AbstractUnsignedTx (..),
) where

import Cardano.Api (FromJSON, ToJSON, Value)
import Cardano.Api qualified as Api
import Data.Aeson qualified as Aeson
import Data.Foldable (foldl)
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Exts (IsList (toList))
import GHC.Generics (Generic)
import Ledger (
  Extended (Finite),
  Interval (Interval),
  LowerBound (LowerBound),
  PubKey,
  Slot,
  UpperBound (UpperBound),
  cardanoPubKeyHash,
  pubKeyHash,
 )

newtype UnsignedTx = UnsignedTx
  { txUnsigned :: Api.TxBody Api.ConwayEra
  }
  deriving newtype (Show, Eq)

newtype AbstractUnsignedTx = AbstractUnsignedTx
  { abstractTxUnsigned :: UnsignedTx
  }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance ToJSON UnsignedTx where
  toJSON (UnsignedTx body) =
    Aeson.toJSON (Api.serialiseToTextEnvelope Nothing body)

instance FromJSON UnsignedTx where
  parseJSON v = do
    envelope <- Aeson.parseJSON v
    case Api.deserialiseFromTextEnvelope @(Api.TxBody Api.ConwayEra) envelope of
      Left err -> fail (show err)
      Right body -> pure (UnsignedTx body)
