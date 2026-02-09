{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.Session.Steps.Submitting.Persistence.Events (
  EventHistory (..),
  loadHistory,
  load,
  persist,
) where

import Cardano.Api qualified as Api
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Word (Word8)
import Path (Dir, Path, reldir, relfile, toFilePath, (</>))
import Path.IO (ensureDir)
import WBPS.Adapter.Monad.Control (whenNothingThrow)
import WBPS.Adapter.Path (readFrom, writeTo)
import WBPS.Core.Failure (WBPSFailure (SessionBlindSignatureNotFound, SessionSubmittedTxNotFound, SessionTxSignatureNotFound))
import WBPS.Core.Registration.FetchAccounts (loadRegistered)
import WBPS.Core.Registration.Registered (
  Registered (Registered, registrationId),
 )
import WBPS.Core.Session.Persistence.FileScheme (deriveExistingSessionDirectoryFrom, deriveSessionDirectoryFrom)
import WBPS.Core.Session.SessionId (SessionId (SessionId, commitmentId, registrationId))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (
  AbstractUnsignedTx (AbstractUnsignedTx),
  PrivateTxInputs (PrivateTxInputs),
  UnsignedTx (txUnsigned),
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (Commitment (Commitment, id))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (
  CircuitMessage (CircuitMessage, message, private, public),
  Message (Message),
  MessageBits (MessageBits),
  MessageParts (MessageParts, message, private, public),
  PreparedMessage (PreparedMessage, circuit, parts),
  PrivateMessage (PrivateMessage),
  PublicMessage (PublicMessage),
 )
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (
  CommitmentDemonstrated (CommitmentDemonstrated, commitment, preparedMessage),
 )
import WBPS.Core.Session.Steps.Demonstration.Persistence.Events qualified as Demonstrated
import WBPS.Core.Session.Steps.Proving.Persistence.Events qualified as Proved
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved)
import WBPS.Core.Session.Steps.Submitting.Artefacts.SubmittedTx (SubmittedTx (SubmittedTx))
import WBPS.Core.Session.Steps.Submitting.Submitted (
  CommitmentSubmitted (CommitmentSubmitted, blindSignature, submittedTx, txId, txSignature),
 )
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme
import WBPS.Core.Setup.Circuit.Parameters (
  CircuitMessageMaxSize (CircuitMessageMaxSize),
  CircuitParameters (CircuitParameters, messageSize, txInputSize),
  CircuitTxInputSize (CircuitTxInputSize),
  MessagePrivatePartOffsetBits (MessagePrivatePartOffsetBits),
  MessagePrivatePartSizeBits (MessagePrivatePartSizeBits),
  messagePrivatePartOffset,
  messagePrivatePartSize,
 )

data EventHistory = EventHistory
  { registered :: Registered
  , demonstrated :: CommitmentDemonstrated
  , proved :: CommitmentProved
  , submitted :: CommitmentSubmitted
  }
  deriving (Eq, Show)

loadHistory ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  SessionId ->
  m EventHistory
loadHistory sessionId@SessionId {registrationId} = do
  sessionDirectory <- deriveExistingSessionDirectoryFrom sessionId
  EventHistory
    <$> loadRegistered registrationId
    <*> Demonstrated.load sessionDirectory sessionId
    <*> Proved.load sessionDirectory sessionId
    <*> load sessionDirectory sessionId

load ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  Path b Dir ->
  SessionId ->
  m CommitmentSubmitted
load sessionDirectory sessionId = do
  submitting <- asks (FileScheme.submitting . FileScheme.session . FileScheme.account)
  let submittedDirectory = sessionDirectory </> [reldir|submitted|]
  blindSignature <-
    readFrom (submittedDirectory </> FileScheme.blindSignature submitting)
      >>= whenNothingThrow [SessionBlindSignatureNotFound sessionId]
  txSignature <-
    readFrom (submittedDirectory </> FileScheme.txSignature submitting)
      >>= whenNothingThrow [SessionTxSignatureNotFound sessionId]
  submittedTx <-
    readFrom (submittedDirectory </> FileScheme.submittedTx submitting)
      >>= whenNothingThrow [SessionSubmittedTxNotFound sessionId]
  let SubmittedTx (Api.Tx txBody _) = submittedTx
  let txId = Api.getTxId txBody
  pure CommitmentSubmitted {blindSignature, txSignature, submittedTx, txId}

persist ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  Registered ->
  CommitmentDemonstrated ->
  CommitmentSubmitted ->
  m CommitmentSubmitted
persist
  Registered {registrationId}
  demonstrated@CommitmentDemonstrated {commitment = Commitment {id = commitmentId}}
  event@CommitmentSubmitted {blindSignature, txSignature, submittedTx} = do
    sessionDirectory <- deriveSessionDirectoryFrom (SessionId {..})
    ensureDir sessionDirectory
    submitting <- asks (FileScheme.submitting . FileScheme.session . FileScheme.account)
    let submittedDirectory = sessionDirectory </> [reldir|submitted|]
    writeTo (submittedDirectory </> FileScheme.blindSignature submitting) blindSignature
    writeTo (submittedDirectory </> FileScheme.txSignature submitting) txSignature
    writeTo (submittedDirectory </> FileScheme.submittedTx submitting) submittedTx
    writeSessionReport sessionDirectory (SessionId {..}) demonstrated event
    return event

writeSessionReport ::
  MonadIO m =>
  Path b Dir ->
  SessionId ->
  CommitmentDemonstrated ->
  CommitmentSubmitted ->
  m ()
writeSessionReport sessionDirectory sessionId demonstrated submitted = do
  let report = renderSessionReport sessionId demonstrated submitted
      reportPath = sessionDirectory </> [relfile|tx-report.txt|]
  liftIO $ TextIO.writeFile (toFilePath reportPath) report

renderSessionReport ::
  SessionId ->
  CommitmentDemonstrated ->
  CommitmentSubmitted ->
  Text
renderSessionReport SessionId {registrationId, commitmentId} CommitmentDemonstrated {preparedMessage} CommitmentSubmitted {submittedTx, txId} =
  Text.unlines $
    header
      <> circuitSection
      <> unsignedSection
      <> splitSection
      <> publicSection
      <> submittedSection
  where
    tshow :: Show a => a -> Text
    tshow = Text.pack . show

    CircuitParameters {messageSize, txInputSize} = def
    CircuitMessageMaxSize maxBits = messageSize
    CircuitTxInputSize inputCount = txInputSize
    MessagePrivatePartOffsetBits offsetBits = messagePrivatePartOffset
    MessagePrivatePartSizeBits privateWindowBits = messagePrivatePartSize txInputSize

    maxBytes :: Int
    maxBytes = maxBits `div` 8

    offsetBytes :: Int
    offsetBytes = offsetBits `div` 8

    privateBytes :: Int
    privateBytes = privateWindowBits `div` 8

    PreparedMessage
      { circuit =
        CircuitMessage
          { message = MessageBits messageBits
          , public = MessageBits publicBits
          , private = MessageBits privateMessageBits
          }
      , parts =
        MessageParts
          { message = Message unsignedTx
          , public = PublicMessage (AbstractUnsignedTx abstractUnsignedTx)
          , private = PrivateMessage (PrivateTxInputs privateInputs)
          }
      } = preparedMessage

    unsignedCbor = Api.serialiseToCBOR (txUnsigned unsignedTx)
    unsignedBytes = BS.length unsignedCbor
    unsignedBits = unsignedBytes * 8
    unsignedContent = renderTxBodyContent (Api.getTxBodyContent (txUnsigned unsignedTx))

    publicCbor = Api.serialiseToCBOR (txUnsigned abstractUnsignedTx)
    publicBytes = BS.length publicCbor
    publicContent = renderTxBodyContent (Api.getTxBodyContent (txUnsigned abstractUnsignedTx))

    SubmittedTx submitted = submittedTx
    submittedCbor = Api.serialiseToCBOR submitted
    submittedBytes = BS.length submittedCbor
    submittedContent = renderTxBodyContent (Api.getTxBodyContent (Api.getTxBody submitted))

    paddingBits =
      if maxBits > unsignedBits
        then maxBits - unsignedBits
        else 0

    messageBitsLen = BS.length messageBits
    publicBitsLen = BS.length publicBits
    privateBitsLen = BS.length privateMessageBits

    messageOnes = countOnes messageBits
    publicOnes = countOnes publicBits
    privateOnes = countOnes privateMessageBits

    privateInputsCount = NE.length privateInputs

    header =
      [ "Session Tx Report"
      , "SessionId: " <> tshow commitmentId
      , "RegistrationId: " <> tshow registrationId
      , ""
      ]

    circuitSection =
      [ "Circuit parameters:"
      , "  message size: " <> tshow maxBits <> " bits (" <> tshow maxBytes <> " bytes)"
      , "  tx inputs: " <> tshow inputCount
      , "  private window offset: " <> tshow offsetBits <> " bits (" <> tshow offsetBytes <> " bytes)"
      , "  private window size: " <> tshow privateWindowBits <> " bits (" <> tshow privateBytes <> " bytes)"
      , "  bit order: LSB-first per byte"
      , ""
      ]

    unsignedSection =
      [ "Unsigned tx (padded to circuit):"
      , "  cbor bytes: " <> tshow unsignedBytes
      , "  cbor bits: " <> tshow unsignedBits
      , "  padding bits added: " <> tshow paddingBits
      , "  tx body content:"
      ]
        <> indentLines 4 unsignedContent
        <> [""]

    splitSection =
      [ "Public/private split (from circuit message bits):"
      , "  message bits: " <> tshow messageBitsLen <> " (ones: " <> tshow messageOnes <> ")"
      , "  public bits:  " <> tshow publicBitsLen <> " (ones: " <> tshow publicOnes <> ")"
      , "  private bits: " <> tshow privateBitsLen <> " (ones: " <> tshow privateOnes <> ")"
      , "  private window byte range: [" <> tshow offsetBytes <> ".." <> tshow (offsetBytes + privateBytes) <> ")"
      , "  private inputs count: " <> tshow privateInputsCount
      , ""
      ]

    publicSection =
      [ "Public message (inputs stripped):"
      , "  cbor bytes: " <> tshow publicBytes
      , "  tx body content:"
      ]
        <> indentLines 4 publicContent
        <> [""]

    submittedSection =
      [ "Submitted tx (signed):"
      , "  txId: " <> tshow txId
      , "  cbor bytes: " <> tshow submittedBytes
      , "  tx body content:"
      ]
        <> indentLines 4 submittedContent

    countOnes :: BS.ByteString -> Int
    countOnes = BS.foldl' (\acc w -> if w == (1 :: Word8) then acc + 1 else acc) 0

    indentLines :: Int -> [Text] -> [Text]
    indentLines n =
      let pad = Text.replicate n " "
       in map (pad <>)

    renderTxBodyContent :: Api.IsShelleyBasedEra era => Api.TxBodyContent build era -> [Text]
    renderTxBodyContent content =
      concat
        [ renderListField "inputs" (Api.txIns content)
        , renderField "collateral inputs" (Api.txInsCollateral content)
        , renderField "reference inputs" (Api.txInsReference content)
        , renderTxOuts "outputs" (Api.txOuts content)
        , renderField "total collateral" (Api.txTotalCollateral content)
        , renderField "return collateral" (Api.txReturnCollateral content)
        , renderField "fee" (Api.txFee content)
        , renderField "validity lower bound" (Api.txValidityLowerBound content)
        , renderField "validity upper bound" (Api.txValidityUpperBound content)
        , renderField "metadata" (Api.txMetadata content)
        , renderField "aux scripts" (Api.txAuxScripts content)
        , renderField "extra key witnesses" (Api.txExtraKeyWits content)
        , renderField "protocol params" (Api.txProtocolParams content)
        , renderField "withdrawals" (Api.txWithdrawals content)
        , renderField "certificates" (Api.txCertificates content)
        , renderField "update proposal" (Api.txUpdateProposal content)
        , renderField "mint" (Api.txMintValue content)
        , renderField "script validity" (Api.txScriptValidity content)
        , renderMaybeField "proposal procedures" (Api.txProposalProcedures content)
        , renderMaybeField "voting procedures" (Api.txVotingProcedures content)
        , renderMaybeField "current treasury value" (Api.txCurrentTreasuryValue content)
        , renderMaybeField "treasury donation" (Api.txTreasuryDonation content)
        ]

    renderField :: Show a => Text -> a -> [Text]
    renderField label value =
      [label <> ": " <> tshow value]

    renderMaybeField :: Show a => Text -> Maybe a -> [Text]
    renderMaybeField label value =
      [label <> ": " <> maybe "none" tshow value]

    renderListField :: Show a => Text -> [a] -> [Text]
    renderListField label values =
      case values of
        [] ->
          [label <> ": []"]
        _ ->
          [label <> ":"] <> indentLines 2 (zipWith renderIndexed [1 :: Int ..] values)

    renderTxOuts :: Text -> [Api.TxOut ctx era] -> [Text]
    renderTxOuts label values =
      case values of
        [] ->
          [label <> ": []"]
        _ ->
          [label <> ":"]
            <> indentLines 2 (concat (zipWith renderTxOut [1 :: Int ..] values))

    renderIndexed :: Show a => Int -> a -> Text
    renderIndexed index value =
      tshow index <> ") " <> tshow value

    renderTxOut :: Int -> Api.TxOut ctx era -> [Text]
    renderTxOut index (Api.TxOut address value datum referenceScript) =
      [ "output " <> tshow index <> ":"
      , "  address: " <> tshow address
      , "  value: " <> tshow value
      , "  datum: " <> tshow datum
      , "  reference script: " <> tshow referenceScript
      ]
