
{-# LANGUAGE RecordWildCards #-}
module Raft.Logging1 where
import Protolude
import Data.Text.Lazy.Builder
import qualified System.IO as IO
import Katip
import Katip.Core
import Katip.Scribes.Handle
import Katip.Format.Time (formatAsLogTime)

defaultLogEnv = do
    handleScribe <- Katip.mkHandleScribeWithFormatter logFormatter Katip.ColorIfTerminal IO.stdout Katip.DebugS Katip.V2
    env <- Katip.initLogEnv "Raft" "production"
    Katip.registerScribe "stdout" handleScribe Katip.defaultScribeSettings env

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

