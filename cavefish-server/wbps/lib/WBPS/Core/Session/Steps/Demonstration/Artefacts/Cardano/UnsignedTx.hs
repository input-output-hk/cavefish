module WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (
  PrivateTxInputs (..),
  UnsignedTx (..),
  AbstractUnsignedTx (..),
  extractPrivateElements,
  randomizeTx,
) where

import Cardano.Api (FromJSON, ToJSON)
import Cardano.Api qualified as Api
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.TxIn qualified as LedgerTxIn
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity (Identity), runIdentity)
import Crypto.Random (getRandomBytes)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Word (Word64)

newtype UnsignedTx = UnsignedTx
  { txUnsigned :: Api.TxBody Api.ConwayEra
  }
  deriving newtype (Show, Eq)

newtype PrivateTxInputs = PrivateTxInputs
  { txInputs :: NonEmpty Api.TxIn
  }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

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

extractPrivateElements :: UnsignedTx -> (PrivateTxInputs, AbstractUnsignedTx)
extractPrivateElements (UnsignedTx txBody@(Api.ShelleyTxBody era body scripts scriptData metadata validity)) =
  ( privateTxInputsFromTxBody txBody
  , AbstractUnsignedTx . UnsignedTx $
      Api.ShelleyTxBody era (setInputs mempty body) scripts scriptData metadata validity
  )

privateTxInputsFromTxBody :: Api.TxBody Api.ConwayEra -> PrivateTxInputs
privateTxInputsFromTxBody txBody =
  case NE.nonEmpty (map fst (Api.txIns (Api.getTxBodyContent txBody))) of
    Nothing -> error "toAbstractUnsignedTx: empty tx inputs"
    Just inputs -> PrivateTxInputs inputs

setInputs ::
  Ledger.EraTxBody era =>
  Set LedgerTxIn.TxIn ->
  Ledger.TxBody era ->
  Ledger.TxBody era
setInputs ins = runIdentity . Ledger.inputsTxBodyL (\_ -> Identity ins)

-- should add aux in metadata
randomizeTx :: MonadIO m => UnsignedTx -> m UnsignedTx
randomizeTx (UnsignedTx (Api.ShelleyTxBody Api.ShelleyBasedEraConway body scripts scriptData mAux validity)) = do
  auxValue <- liftIO $ getRandomBytes 16
  let auxData' = addAuxMetadata auxValue mAux
  pure . UnsignedTx $ Api.ShelleyTxBody Api.ShelleyBasedEraConway body scripts scriptData auxData' validity

cavefishAuxLabel :: Word64
cavefishAuxLabel = 1991

addAuxMetadata ::
  ByteString ->
  Maybe (Ledger.TxAuxData (Api.ShelleyLedgerEra Api.ConwayEra)) ->
  Maybe (Ledger.TxAuxData (Api.ShelleyLedgerEra Api.ConwayEra))
addAuxMetadata randomBytes = \case
  Nothing ->
    Api.toAuxiliaryData Api.ShelleyBasedEraConway (Api.TxMetadataInEra Api.ShelleyBasedEraConway singleEntryMetadata) Api.TxAuxScriptsNone
  Just auxData ->
    let updatedMetadata =
          Map.insert cavefishAuxLabel (Api.toShelleyMetadatum auxEntry) (Ledger.atadMetadata auxData)
        existingScripts = toList (Ledger.getAlonzoTxAuxDataScripts auxData)
     in Just (Ledger.mkAlonzoTxAuxData updatedMetadata existingScripts)
  where
    singleEntryMetadata =
      Api.makeTransactionMetadata (Map.singleton cavefishAuxLabel auxEntry)
    auxEntry =
      Api.TxMetaMap
        [ (Api.TxMetaText "cavefish_auxilaire", Api.TxMetaBytes randomBytes)
        ]
