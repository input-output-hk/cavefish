{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.Performance (
  PerfEvent (..),
  PerfSummary (..),
  PerfTags,
  appendPerfEvent,
  withPerfEvent,
  withPerfEventFromResult,
  withPerfEventIO,
  readPerfEvents,
  renderPerfReport,
  performanceReportPath,
  writePerformanceReport,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.List (foldl', partition, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)
import GHC.Generics (Generic)
import Path (Abs, File, Path, parent, relfile, toFilePath, (</>))
import Path.IO (doesFileExist, ensureDir)
import System.Clock qualified as Clock
import Text.Printf (printf)

type PerfTags = Map Text Text

data PerfEvent = PerfEvent
  { label :: Text
  , startedAt :: UTCTime
  , endedAt :: UTCTime
  , durationMs :: Double
  , tags :: PerfTags
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data PerfSummary = PerfSummary
  { label :: Text
  , count :: Int
  , totalMs :: Double
  , minMs :: Double
  , maxMs :: Double
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

appendPerfEvent :: Path Abs File -> PerfEvent -> IO ()
appendPerfEvent path event = do
  ensureDir (parent path)
  BL.appendFile (toFilePath path) (encode event <> "\n")

withPerfEventIO :: Path Abs File -> Text -> PerfTags -> IO a -> IO a
withPerfEventIO logPath label tags action = do
  startedAt <- getCurrentTime
  startedMono <- Clock.getTime Clock.Monotonic
  result <- action
  endedAt <- getCurrentTime
  endedMono <- Clock.getTime Clock.Monotonic
  let durationMs = toMilliseconds (endedMono - startedMono)
  appendPerfEvent logPath PerfEvent {..}
  pure result

withPerfEvent :: MonadIO m => Path Abs File -> Text -> PerfTags -> m a -> m a
withPerfEvent logPath label tags action = do
  startedAt <- liftIO getCurrentTime
  startedMono <- liftIO $ Clock.getTime Clock.Monotonic
  result <- action
  endedAt <- liftIO getCurrentTime
  endedMono <- liftIO $ Clock.getTime Clock.Monotonic
  let durationMs = toMilliseconds (endedMono - startedMono)
  liftIO $ appendPerfEvent logPath PerfEvent {..}
  pure result

withPerfEventFromResult :: MonadIO m => Path Abs File -> Text -> PerfTags -> (a -> PerfTags) -> m a -> m a
withPerfEventFromResult logPath label baseTags mkTags action = do
  startedAt <- liftIO getCurrentTime
  startedMono <- liftIO $ Clock.getTime Clock.Monotonic
  result <- action
  endedAt <- liftIO getCurrentTime
  endedMono <- liftIO $ Clock.getTime Clock.Monotonic
  let durationMs = toMilliseconds (endedMono - startedMono)
      tags = baseTags `Map.union` mkTags result
  liftIO $ appendPerfEvent logPath PerfEvent {..}
  pure result

readPerfEvents :: Path Abs File -> IO [PerfEvent]
readPerfEvents path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      bytes <- BL.readFile (toFilePath path)
      pure $ mapMaybe decode (BL8.lines bytes)

renderPerfReport :: [PerfEvent] -> Text
renderPerfReport events =
  if null events
    then "No performance events recorded."
    else
      let totalMs = sum (map durationMs events)
          (startAt, endAt) = eventWindow events
          spanMs :: Double
          spanMs = maybe 0 (realToFrac . (* 1000)) (diffUTCTime <$> endAt <*> startAt)
          header =
            "Performance report (" <> Text.pack (show (length events)) <> " events)"
          windowLine =
            "Window: "
              <> formatUtc startAt
              <> " -> "
              <> formatUtc endAt
              <> " (span "
              <> Text.pack (printf "%.2f" spanMs)
              <> " ms)"
          totalsLine =
            "Total recorded time: " <> Text.pack (printf "%.2f" totalMs) <> " ms"
          tableHeader =
            printf
              "%-40s %5s %10s %10s %10s %10s %7s"
              ("label" :: String)
              ("count" :: String)
              ("total_ms" :: String)
              ("avg_ms" :: String)
              ("min_ms" :: String)
              ("max_ms" :: String)
              ("%total" :: String)
          reportLines =
            [ header
            , windowLine
            , totalsLine
            , ""
            ]
              <> renderSummarySection "Overall summary:" totalMs events tableHeader
              <> [""]
              <> renderAccountSections events tableHeader
              <> [""]
              <> renderUnattributedSection events tableHeader
              <> [""]
              <> renderSlowSection events
       in Text.unlines reportLines

performanceReportPath :: Path Abs File -> Path Abs File
performanceReportPath logPath =
  parent logPath </> [relfile|performance-report.txt|]

writePerformanceReport :: Path Abs File -> IO Text
writePerformanceReport logPath = do
  events <- readPerfEvents logPath
  let report = renderPerfReport events
  let reportPath = performanceReportPath logPath
  ensureDir (parent reportPath)
  TIO.writeFile (toFilePath reportPath) report
  pure report

summarize :: [PerfEvent] -> [PerfSummary]
summarize events =
  let summaryMap =
        Map.fromListWith
          mergeSummary
          [ (eventLabel, PerfSummary {label = eventLabel, count = 1, totalMs = durationMs, minMs = durationMs, maxMs = durationMs})
          | PerfEvent {label = eventLabel, durationMs} <- events
          ]
   in sortOn (\summary -> Text.unpack (summary.label)) (Map.elems summaryMap)

mergeSummary :: PerfSummary -> PerfSummary -> PerfSummary
mergeSummary left right =
  PerfSummary
    { label = left.label
    , count = count left + count right
    , totalMs = totalMs left + totalMs right
    , minMs = min (minMs left) (minMs right)
    , maxMs = max (maxMs left) (maxMs right)
    }

formatSummaryWithPct :: Double -> PerfSummary -> String
formatSummaryWithPct totalAllMs PerfSummary {label, count, totalMs, minMs, maxMs} =
  let avgMs = totalMs / fromIntegral count
      pct =
        if totalAllMs <= 0
          then 0
          else (totalMs / totalAllMs) * 100
   in printf
        "%-40s %5d %10.2f %10.2f %10.2f %10.2f %6.1f"
        (Text.unpack label)
        count
        totalMs
        avgMs
        minMs
        maxMs
        pct

formatSlowEvent :: PerfEvent -> String
formatSlowEvent PerfEvent {label, durationMs, tags} =
  printf
    "%-40s %10.2f %s"
    (truncateString 40 (Text.unpack label))
    durationMs
    (truncateString 80 (Text.unpack (formatTags tags)))

formatTags :: PerfTags -> Text
formatTags tagMap
  | Map.null tagMap = Text.pack "-"
  | otherwise =
      Text.intercalate
        (Text.pack ", ")
        [key <> Text.pack "=" <> value | (key, value) <- Map.toAscList tagMap]

eventWindow :: [PerfEvent] -> (Maybe UTCTime, Maybe UTCTime)
eventWindow [] = (Nothing, Nothing)
eventWindow xs =
  let starts = map startedAt xs
      ends = map endedAt xs
   in (Just (minimum starts), Just (maximum ends))

formatUtc :: Maybe UTCTime -> Text
formatUtc Nothing = Text.pack "n/a"
formatUtc (Just time) =
  Text.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q UTC" time)

truncateString :: Int -> String -> String
truncateString limit value
  | length value <= limit = value
  | limit <= 3 = take limit value
  | otherwise = take (limit - 3) value <> "..."

registrationTagKey :: Text
registrationTagKey = Text.pack "registrationId"

sessionTagKey :: Text
sessionTagKey = Text.pack "sessionId"

txBodyBytesTagKey :: Text
txBodyBytesTagKey = Text.pack "tx_body_bytes"

registrationLabels :: [Text]
registrationLabels =
  map
    Text.pack
    [ "endpoint.register"
    , "snarkjs.generate.proving.key"
    , "snarkjs.generate.public.verification.context"
    ]

renderSummarySection :: Text -> Double -> [PerfEvent] -> String -> [Text]
renderSummarySection title totalAllMs events tableHeader =
  if null events
    then [title, Text.pack "  (no events)"]
    else
      let sortByAvg =
            sortOn
              ( Down
                  . (\PerfSummary {totalMs, count} -> totalMs / fromIntegral count)
              )
          summaries = summarize events
          (endpointSummaries, otherSummaries) =
            partition
              (\PerfSummary {label} -> "endpoint." `Text.isPrefixOf` label)
              summaries
          endpointRows = map (formatSummaryWithPct totalAllMs) (sortByAvg endpointSummaries)
          otherRows = map (formatSummaryWithPct totalAllMs) (sortByAvg otherSummaries)
          endpointSection =
            if null endpointRows
              then []
              else
                [Text.pack "  Endpoints:", Text.pack tableHeader]
                  <> map (Text.pack . ("  " <>)) endpointRows
          otherSection =
            if null otherRows
              then []
              else
                [Text.pack "  Other:", Text.pack tableHeader]
                  <> map (Text.pack . ("  " <>)) otherRows
       in [title] <> endpointSection <> otherSection

renderAccountSections :: [PerfEvent] -> String -> [Text]
renderAccountSections events tableHeader =
  let accounts = groupByTag registrationTagKey events
      unattributed = filter (\ev -> not (hasTag registrationTagKey ev) && not (hasTag sessionTagKey ev)) events
      sessionCount = length (groupByTag sessionTagKey events)
      singleSessionAttribution = sessionCount == 1 && Map.size accounts == 1
   in if Map.null accounts
        then ["Accounts: (no registration-tagged events)"]
        else
          ["Accounts:"]
            <> concatMap (renderAccountSection tableHeader unattributed singleSessionAttribution) (Map.toAscList accounts)

renderAccountSection :: String -> [PerfEvent] -> Bool -> (Text, [PerfEvent]) -> [Text]
renderAccountSection _tableHeader unattributed singleSessionAttribution (registrationId, accountEvents) =
  let accountTotal = sumDuration accountEvents
      regEvents = filter isRegistrationEvent accountEvents
      sessionEvents = filter (hasTag sessionTagKey) accountEvents
      preSessionEvents = filter isPreSessionEvent accountEvents
      verifyEvents = filter isVerifyEvent accountEvents
      txBuildUnattributed = filter isTxBuildUnattributed unattributed
      attributableUnattributed =
        if singleSessionAttribution
          then filter (\PerfEvent {label = eventLabel} -> eventLabel `elem` singleSessionAttributableLabels) unattributed
          else []
      sessions = groupByTag sessionTagKey sessionEvents
      sessionList = Map.toAscList (attachTxBuild sessions txBuildUnattributed)
      (sessionListWithPre, unscopedPre) =
        if length sessionList == 1
          then
            let (sid, evs) = head sessionList
             in ([(sid, evs <> preSessionEvents <> verifyEvents <> attributableUnattributed)], [])
          else (sessionList, preSessionEvents <> verifyEvents)
      sessionSections = concatMap renderSessionFlow sessionListWithPre
      unscopedSection =
        if null unscopedPre
          then []
          else renderUnscopedLifecycle unscopedPre
   in [ "Account: " <> registrationId <> " (total " <> Text.pack (printf "%.2f" accountTotal) <> " ms)"
      ]
        <> renderRegistrationFlow regEvents
        <> if null sessionList
          then [Text.pack "  Sessions: (none)"]
          else
            (Text.pack "  Sessions:" : sessionSections)
              <> unscopedSection

renderRegistrationFlow :: [PerfEvent] -> [Text]
renderRegistrationFlow regEvents =
  let total = sumDuration regEvents
      steps =
        [ (Text.pack "1) SP.Register (Endpoint)", registrationStepLabels)
        ]
   in if null regEvents
        then [Text.pack "  Registration: (no events)"]
        else
          [ Text.pack ("  Registration (total " <> printf "%.2f" total <> " ms):")
          ]
            <> concatMap (\(title, labels) -> renderFlowStep (Text.pack "    " <> title) labels regEvents total) steps

renderSessionFlow :: (Text, [PerfEvent]) -> [Text]
renderSessionFlow (sessionId, sessionEvents) =
  let sessionTotal = sumDuration sessionEvents
      steps =
        [ (Text.pack "1) SP.Demonstrate (Endpoint)", demonstrateStepLabels)
        , (Text.pack "2) SP.Prove (Endpoint)", proveStepLabels)
        , (Text.pack "3) User.Verify", verifyStepLabels)
        , (Text.pack "4) SP.Submit (Endpoint)", submitStepLabels)
        ]
   in [ Text.pack ("  Session " <> Text.unpack (truncateText 80 sessionId) <> " (total " <> printf "%.2f" sessionTotal <> " ms)")
      ]
        <> concatMap (\(title, labels) -> renderFlowStep (Text.pack "    " <> title) labels sessionEvents sessionTotal) steps

renderUnscopedLifecycle :: [PerfEvent] -> [Text]
renderUnscopedLifecycle events =
  let total = sumDuration events
      steps =
        [ (Text.pack "SP.Demonstrate (Endpoint)", demonstrateStepLabels)
        , (Text.pack "SP.Prove (Endpoint)", proveStepLabels)
        , (Text.pack "User.Verify", verifyStepLabels)
        , (Text.pack "SP.Submit (Endpoint)", submitStepLabels)
        ]
   in [ Text.pack ("  Session lifecycle (unscoped) (total " <> printf "%.2f" total <> " ms):")
      ]
        <> concatMap (\(title, labels) -> renderFlowStep (Text.pack "    " <> title) labels events total) steps

renderUnattributedSection :: [PerfEvent] -> String -> [Text]
renderUnattributedSection events tableHeader =
  let unattributedAll =
        filter (\ev -> not (hasTag registrationTagKey ev) && not (hasTag sessionTagKey ev)) events
      sessionCount = length (groupByTag sessionTagKey events)
      accountCount = Map.size (groupByTag registrationTagKey events)
      singleSessionAttribution = sessionCount == 1 && accountCount == 1
      unattributed =
        if singleSessionAttribution
          then filter (\PerfEvent {label = eventLabel} -> eventLabel `notElem` singleSessionAttributableLabels) unattributedAll
          else unattributedAll
   in if null unattributed
        then []
        else
          [ "Unattributed events (no registrationId/sessionId tags):"
          ]
            <> renderSummarySection (Text.pack "  Summary:") (sumDuration unattributed) unattributed tableHeader

renderSlowSection :: [PerfEvent] -> [Text]
renderSlowSection events =
  let slowHeader =
        printf "%-40s %10s %s" ("slowest_event" :: String) ("ms" :: String) ("tags" :: String)
      slowRows = map formatSlowEvent (take 5 (sortOn (Down . durationMs) events))
   in [Text.pack "Slowest events:", Text.pack slowHeader] <> map Text.pack slowRows

groupByTag :: Text -> [PerfEvent] -> Map Text [PerfEvent]
groupByTag key events =
  Map.fromListWith
    (++)
    [ (value, [event])
    | event <- events
    , Just value <- [Map.lookup key (tags event)]
    ]

hasTag :: Text -> PerfEvent -> Bool
hasTag key event = Map.member key (tags event)

isRegistrationEvent :: PerfEvent -> Bool
isRegistrationEvent PerfEvent {label = eventLabel} = eventLabel `elem` registrationLabels

isPreSessionEvent :: PerfEvent -> Bool
isPreSessionEvent PerfEvent {label = eventLabel, tags} =
  Map.member registrationTagKey tags
    && not (Map.member sessionTagKey tags)
    && eventLabel `elem` demonstrateStepLabels

isVerifyEvent :: PerfEvent -> Bool
isVerifyEvent PerfEvent {label = eventLabel} = eventLabel `elem` verifyStepLabels

isTxBuildUnattributed :: PerfEvent -> Bool
isTxBuildUnattributed PerfEvent {label = eventLabel, tags} =
  eventLabel == Text.pack "build.transaction"
    && not (Map.member registrationTagKey tags)
    && not (Map.member sessionTagKey tags)

sumDuration :: [PerfEvent] -> Double
sumDuration = sum . map durationMs

truncateText :: Int -> Text -> Text
truncateText limit value
  | Text.length value <= limit = value
  | limit <= 3 = Text.take limit value
  | otherwise = Text.take (limit - 3) value <> Text.pack "..."

registrationStepLabels :: [Text]
registrationStepLabels =
  map Text.pack ["endpoint.register", "snarkjs.generate.proving.key", "snarkjs.generate.public.verification.context"]

demonstrateStepLabels :: [Text]
demonstrateStepLabels =
  map
    Text.pack
    [ "endpoint.demonstrate"
    , "build.transaction"
    , "snarkjs.build.commitment"
    , "snarkjs.export.statement.json"
    ]

proveStepLabels :: [Text]
proveStepLabels =
  map
    Text.pack
    [ "endpoint.prove"
    , "snarkjs.generate.witness"
    , "snarkjs.generate.proof"
    ]

submitStepLabels :: [Text]
submitStepLabels =
  map
    Text.pack
    [ "endpoint.submit"
    , "tx.submit"
    ]

verifyStepLabels :: [Text]
verifyStepLabels = [Text.pack "snarkjs.verify"]

renderFlowStep :: Text -> [Text] -> [PerfEvent] -> Double -> [Text]
renderFlowStep title labels events totalAll =
  let stepEvents = filter (\PerfEvent {label} -> label `elem` labels) events
      endpointLabel = endpointLabelFor title
      endpointEvent = endpointLabel >>= \lbl -> Map.lookup lbl eventMap
      stepTotal =
        case endpointEvent of
          Just PerfEvent {durationMs} -> durationMs
          Nothing -> sumDuration stepEvents
      summaries = summarize stepEvents
      summaryMap = Map.fromList [(s.label, s) | s <- summaries]
      orderedSummaries = mapMaybe (`Map.lookup` summaryMap) labels
      eventMap = Map.fromList [(label, event) | event@PerfEvent {label} <- stepEvents]
      percent =
        if totalAll <= 0
          then 0
          else (stepTotal / totalAll) * 100
      percentWithinStepBase base totalMs =
        if base <= 0
          then 0
          else (totalMs / base) * 100
      headerLine =
        Text.pack
          ( Text.unpack title
              <> ": "
              <> printf "%.2f" stepTotal
              <> " ms ("
              <> printf "%.1f" percent
              <> "%)"
          )
   in if null stepEvents
        then [Text.pack (Text.unpack title <> " (no events)")]
        else
          let filteredDetails =
                case endpointLabel of
                  Just lbl -> filter (\summary -> summary.label /= lbl) orderedSummaries
                  Nothing -> orderedSummaries
              detailTotal = sum (map (.totalMs) filteredDetails)
              remainingMs = max 0 (stepTotal - detailTotal)
              percentBase =
                if detailTotal > 0
                  then detailTotal + remainingMs
                  else stepTotal
              filteredLines =
                map
                  ( \summary ->
                      let extra =
                            case Map.lookup (summary.label) eventMap of
                              Just PerfEvent {tags}
                                | summary.label == Text.pack "build.transaction" ->
                                    case Map.lookup txBodyBytesTagKey tags of
                                      Just bytes -> " (bytes=" <> Text.unpack bytes <> ")"
                                      Nothing -> ""
                              _ -> ""
                          displayLabel = prettyEventLabel summary.label
                          pct = percentWithinStepBase percentBase summary.totalMs
                       in Text.pack
                            ( "      - "
                                <> Text.unpack displayLabel
                                <> ": "
                                <> printf "%.2f" (summary.totalMs)
                                <> " ms ("
                                <> printf "%.1f" pct
                                <> "%)"
                                <> extra
                            )
                  )
                  filteredDetails
              remainingLine =
                if remainingMs <= 0.001 || null filteredDetails
                  then []
                  else
                    [ Text.pack
                        ( "      - remaining: "
                            <> printf "%.2f" remainingMs
                            <> " ms ("
                            <> printf "%.1f" (percentWithinStepBase percentBase remainingMs)
                            <> "%)"
                        )
                    ]
           in headerLine : filteredLines <> remainingLine

attachTxBuild :: Map Text [PerfEvent] -> [PerfEvent] -> Map Text [PerfEvent]
attachTxBuild sessions txBuildEvents =
  let sessionStarts =
        [ (sessionId, earliestStart events)
        | (sessionId, events) <- Map.toAscList sessions
        , not (null events)
        ]
      sortedSessions = sortOn snd sessionStarts
      sortedBuilds = sortOn startedAt txBuildEvents
      assignments = zip sortedSessions sortedBuilds
   in foldl'
        (\acc ((sessionId, _), buildEvent) -> Map.insertWith (<>) sessionId [buildEvent] acc)
        sessions
        assignments

earliestStart :: [PerfEvent] -> UTCTime
earliestStart events = minimum (map startedAt events)

singleSessionAttributableLabels :: [Text]
singleSessionAttributableLabels =
  [ Text.pack "build.transaction"
  , Text.pack "tx.submit"
  , Text.pack "snarkjs.verify"
  ]

prettyEventLabel :: Text -> Text
prettyEventLabel label
  | label == Text.pack "endpoint.demonstrate" = Text.pack "SP.Demonstrate (Endpoint)"
  | label == Text.pack "endpoint.prove" = Text.pack "SP.Prove (Endpoint)"
  | label == Text.pack "endpoint.submit" = Text.pack "SP.Submit (Endpoint)"
  | label == Text.pack "endpoint.register" = Text.pack "SP.Register (Endpoint)"
  | otherwise = label

endpointLabelFor :: Text -> Maybe Text
endpointLabelFor title
  | Text.isPrefixOf (Text.pack "    1) SP.Demonstrate (Endpoint)") title = Just (Text.pack "endpoint.demonstrate")
  | Text.isPrefixOf (Text.pack "    2) SP.Prove (Endpoint)") title = Just (Text.pack "endpoint.prove")
  | Text.isPrefixOf (Text.pack "    4) SP.Submit (Endpoint)") title = Just (Text.pack "endpoint.submit")
  | Text.isPrefixOf (Text.pack "    1) SP.Register (Endpoint)") title = Just (Text.pack "endpoint.register")
  | otherwise = Nothing

toMilliseconds :: Clock.TimeSpec -> Double
toMilliseconds ts = fromIntegral (Clock.toNanoSecs ts) / nsPerMs

nsPerMs :: Double
nsPerMs = 1000000
