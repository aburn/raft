
{-# LANGUAGE RecordWildCards #-}
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

emptyLogEnv :: LogEnv
emptyLogEnv = LogEnv "localhost" 0 "Raft" "" getCurrentTime Map.empty

defaultLogEnv :: LogDest m -> IO LogEnv
defaultLogEnv logDest= do
    handleScribe <-
      case logDest of
        LogStdout -> Katip.mkHandleScribeWithFormatter logFormatter Katip.ColorIfTerminal IO.stdout Katip.DebugS Katip.V2
        LogFile path -> Katip.mkFileScribe path Katip.DebugS Katip.V2

    env <- Katip.initLogEnv "Raft" "production"
    Katip.registerScribe "stdout" handleScribe Katip.defaultScribeSettings env

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

