
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Raft.Logging1 where
import Protolude
import Data.Text.Lazy.Builder

import Data.Time (getCurrentTime)
import qualified System.IO as IO
import qualified Data.Map                 as Map
import Katip
import Katip.Core
import Katip.Scribes.Handle
import Katip.Format.Time (formatAsLogTime)

import Raft.Logging

data KatipEnv = KatipEnv {
    katipLogEnv :: Katip.LogEnv
  , katipContext   :: Katip.LogContexts
  , katipNamespace :: Katip.Namespace
}

localKatipLogEnv :: (LogEnv -> LogEnv) -> KatipEnv -> KatipEnv
localKatipLogEnv f s@KatipEnv{..} = s { katipLogEnv = f katipLogEnv }

localLogContexts :: (LogContexts-> LogContexts) -> KatipEnv -> KatipEnv
localLogContexts f s@KatipEnv{..} = s { katipContext = f katipContext }


localNamespace :: (Namespace -> Namespace) -> KatipEnv -> KatipEnv
localNamespace f s@KatipEnv{..} = s { katipNamespace = f katipNamespace}


emptyLogEnv :: LogEnv
emptyLogEnv = LogEnv "localhost" 0 "Raft" "" getCurrentTime Map.empty

logWithScribe :: (Raft.Logging.Severity -> Text -> IO ()) -> Scribe
logWithScribe f = Scribe logger (return ())
  where
    --logger :: Katip.Item v -> IO ()
    logger i@Katip.Item{..} = f Info $toS $ toLazyText (logFormatter False V2 i)

defaultLogEnv :: MonadIO m => LogDest  -> m LogEnv
defaultLogEnv logDest= do
    handleScribe <-
      case logDest of
        LogWith f -> pure $ logWithScribe f
        LogStdout -> liftIO $ Katip.mkHandleScribeWithFormatter logFormatter Katip.ColorIfTerminal IO.stdout Katip.DebugS Katip.V2
        LogFile path -> liftIO$ Katip.mkFileScribe path Katip.DebugS Katip.V2

    env <- liftIO $ Katip.initLogEnv "Raft" "production"
    liftIO $ Katip.registerScribe "stdout" handleScribe Katip.defaultScribeSettings env

-- | Log formatter with PID and ThreadID removed.
-- Based on https://hackage.haskell.org/package/katip-0.8.1.0/docs/src/Katip.Scribes.Handle.html#bracketFormat
logFormatter :: Katip.LogItem a => Katip.ItemFormatter a
logFormatter withColor verb Item{..} =
  brackets nowStr <>
  brackets (mconcat $ map fromText $ intercalateNs _itemNamespace) <>
  brackets (fromText (renderSeverity' _itemSeverity)) <>
  mconcat ks <>
    maybe mempty (brackets . fromString . locationToString) _itemLoc <>
    fromText " " <> (unLogStr _itemMessage)
  where
    nowStr = fromText (formatAsLogTime _itemTime)
    ks = map brackets $ getKeys verb _itemPayload
    renderSeverity' severity =
      colorBySeverity withColor severity (renderSeverity severity)


logInfo :: Katip.KatipContext m => Text -> m ()
logInfo msg = Katip.logFM Katip.InfoS $ Katip.logStr msg

logDebug :: Katip.KatipContext m => Text -> m ()
logDebug msg = Katip.logFM Katip.DebugS $ Katip.logStr msg

logCritical :: Katip.KatipContext m => Text -> m ()
logCritical msg = Katip.logFM Katip.CriticalS $ Katip.logStr msg


