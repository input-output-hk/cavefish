{-# LANGUAGE OverloadedStrings #-}

import qualified Cardano.Api as Api
import qualified Cardano.Crypto.Seed as Crypto
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as Term
import qualified Codec.CBOR.Write as CBOR
import Control.Monad (forM_, when)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  counts <- either die pure (parseCounts args)
  forM_ counts $ \n -> do
    when (n <= 0) $ die "input count must be > 0"
    body <- either (die . show) pure (buildTxBody n)
    let cborBytes = Api.serialiseToCBOR body
    case inspectInputsWindow n cborBytes of
      Left err -> die err
      Right (offsetBytes, sizeBytes, perInputBytes, overheadBytes) ->
        putStrLn (renderStats n offsetBytes sizeBytes perInputBytes overheadBytes)

parseCounts :: [String] -> Either String [Int]
parseCounts [] = Right [1 .. 5]
parseCounts ["--max", nStr] = do
  n <- parseInt nStr
  Right [1 .. n]
parseCounts xs = traverse parseInt xs

parseInt :: String -> Either String Int
parseInt raw =
  case readMaybe raw of
    Nothing -> Left ("invalid integer: " <> raw)
    Just n -> Right n

buildTxBody :: Int -> Either Api.TxBodyError (Api.TxBody Api.ConwayEra)
buildTxBody n =
  Api.createTransactionBody Api.ShelleyBasedEraConway txBodyContent
  where
    txBodyContent =
      (Api.defaultTxBodyContent Api.ShelleyBasedEraConway)
        { Api.txIns = mkInputs n
        , Api.txOuts =
            [ Api.TxOut
                dummyAddr
                (Api.lovelaceToTxOutValue Api.ShelleyBasedEraConway oneLovelace)
                Api.TxOutDatumNone
                Api.ReferenceScriptNone
            ]
        }
    oneLovelace = Api.quantityToLovelace (Api.Quantity 1)

mkInputs :: Int -> [(Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxTxIn Api.ConwayEra))]
mkInputs n =
  [ (Api.TxIn (dummyTxId i) (Api.TxIx 0), Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending))
  | i <- [0 .. n - 1]
  ]

dummyTxId :: Int -> Api.TxId
dummyTxId i =
  case Api.deserialiseFromRawBytes Api.AsTxId (txIdBytes i) of
    Right v -> v
    Left err -> error (show err)

txIdBytes :: Int -> BS.ByteString
txIdBytes i =
  let bytes = unfoldBytes (fromIntegral i)
      padLen = max 0 (32 - BS.length bytes)
      padded = bytes <> BS.replicate padLen 0
   in BS.take 32 padded
  where
    unfoldBytes :: Integer -> BS.ByteString
    unfoldBytes n
      | n <= 0 = BS.empty
      | otherwise =
          let byte = fromIntegral (n .&. 0xff) :: Word8
           in BS.cons byte (unfoldBytes (n `shiftR` 8))

dummyAddr :: Api.AddressInEra Api.ConwayEra
dummyAddr =
  let seed = Crypto.mkSeedFromBytes (BS.replicate 32 7)
      dummySk = Api.deterministicSigningKey Api.AsPaymentKey seed
      dummyVk = Api.getVerificationKey dummySk
   in Api.makeShelleyAddressInEra
        Api.ShelleyBasedEraConway
        Api.Mainnet
        (Api.PaymentCredentialByKey (Api.verificationKeyHash dummyVk))
        Api.NoStakeAddress

inspectInputsWindow :: Int -> BS.ByteString -> Either String (Int, Int, Maybe Int, Maybe Int)
inspectInputsWindow n cborBytes = do
  term <- decodeTerm cborBytes
  inputsTerm <- extractInputsTerm term
  let encodedInputs = encodeTerm inputsTerm
  offsetBytes <- findOffset encodedInputs cborBytes
  let sizeBytes = BS.length encodedInputs
      perInputBytes = inputElementSize inputsTerm
      overheadBytes = (\per -> sizeBytes - per * n) <$> perInputBytes
  Right (offsetBytes, sizeBytes, perInputBytes, overheadBytes)

decodeTerm :: BS.ByteString -> Either String Term.Term
decodeTerm bs =
  case CBOR.deserialiseFromBytes Term.decodeTerm (BL.fromStrict bs) of
    Left err -> Left (show err)
    Right (rest, term)
      | BL.null rest -> Right term
      | otherwise -> Left "unexpected trailing bytes after decoding CBOR"

extractInputsTerm :: Term.Term -> Either String Term.Term
extractInputsTerm term = do
  body <- extractTxBody term
  pairs <- case body of
    Term.TMap kvs -> Right kvs
    Term.TMapI kvs -> Right kvs
    _ -> Left "unexpected tx body: expected CBOR map"
  case find (isKeyZero . fst) pairs of
    Just (_, v) -> Right v
    Nothing -> Left "inputs key (0) not found in tx body map"

extractTxBody :: Term.Term -> Either String Term.Term
extractTxBody term =
  case term of
    Term.TList (body : _) -> Right body
    Term.TListI (body : _) -> Right body
    _ -> Left "unexpected tx CBOR: expected a list for the tx body wrapper"

isKeyZero :: Term.Term -> Bool
isKeyZero term =
  case term of
    Term.TInt n -> n == 0
    Term.TInteger n -> n == 0
    _ -> False

encodeTerm :: Term.Term -> BS.ByteString
encodeTerm = CBOR.toStrictByteString . Term.encodeTerm

findOffset :: BS.ByteString -> BS.ByteString -> Either String Int
findOffset needle haystack =
  let (prefix, rest) = BS.breakSubstring needle haystack
   in if BS.null rest
        then Left "inputs bytes not found in CBOR encoding"
        else Right (BS.length prefix)

inputElementSize :: Term.Term -> Maybe Int
inputElementSize term =
  case extractInputsList term of
    Left _ -> Nothing
    Right xs -> (\x -> BS.length (encodeTerm x)) <$> listToMaybe xs

extractInputsList :: Term.Term -> Either String [Term.Term]
extractInputsList term =
  case term of
    Term.TTagged _ inner -> extractInputsList inner
    Term.TList xs -> Right xs
    Term.TListI xs -> Right xs
    _ -> Left "inputs term is not a list"

renderStats :: Int -> Int -> Int -> Maybe Int -> Maybe Int -> String
renderStats n offsetBytes sizeBytes perInputBytes overheadBytes =
  unwords (baseParts <> perInputPart <> overheadPart)
  where
    baseParts =
      [ "inputs=" <> show n
      , "offset_bytes=" <> show offsetBytes
      , "offset_bits=" <> show (offsetBytes * 8)
      , "size_bytes=" <> show sizeBytes
      , "size_bits=" <> show (sizeBytes * 8)
      ]
    perInputPart = maybe [] (\per -> ["per_input_bytes=" <> show per]) perInputBytes
    overheadPart = maybe [] (\over -> ["overhead_bytes=" <> show over]) overheadBytes
