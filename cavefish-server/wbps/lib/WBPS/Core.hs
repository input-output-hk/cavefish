module WBPS.Core (
  SignerKey,
  WbpsPublicKey (..),
) where

import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import WBPS.Adapter.CardanoCryptoClass.Crypto (PublicKey)

type SignerKey = PublicKey Ed25519DSIGN

data WbpsPublicKey = WbpsPublicKey
  { wpkX :: ByteString
  , wpkY :: ByteString
  }
  deriving (Eq, Show, Generic)

instance ToJSON WbpsPublicKey where
  toJSON WbpsPublicKey {wpkX, wpkY} =
    object
      [ "x" .= encodeHex wpkX
      , "y" .= encodeHex wpkY
      ]

instance FromJSON WbpsPublicKey where
  parseJSON = withObject "WbpsPublicKey" $ \o -> do
    xHex <- o .: "x"
    yHex <- o .: "y"
    wpkX <- decodeHex "x" xHex
    wpkY <- decodeHex "y" yHex
    pure WbpsPublicKey {wpkX, wpkY}

encodeHex :: ByteString -> Text
encodeHex = TE.decodeUtf8 . B16.encode

decodeHex :: String -> Text -> Parser ByteString
decodeHex label hexTxt =
  case B16.decode (TE.encodeUtf8 hexTxt) of
    Left err ->
      fail ("invalid " <> label <> " coordinate: " <> err)
    Right bytes ->
      pure bytes
