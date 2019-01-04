{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Raft.Logging where

import Protolude

import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.State (modify')

import Data.Time
import Data.Time.Clock.System

import Raft.NodeState
import Raft.Types

-- | Representation of the logs' destination
data LogDest
  = LogFile FilePath
  | LogStdout
  | NoLogs

-- | Representation of the severity of the logs
data Severity
  = Info
  | Debug
  | Critical
  deriving (Show)

data LogMsg = LogMsg
  { mTime :: Maybe SystemTime
  , severity :: Severity
  , logMsgData :: LogMsgData
  }

data LogMsgData = LogMsgData
  { logMsgNodeId :: NodeId
  , logMsgNodeState :: Mode
  , logMsg :: Text
  } deriving (Show)

logMsgToText :: LogMsg -> Text
logMsgToText (LogMsg mt s d) =
    maybe "" sysTimeToText mt <> "(" <> show s <> ")" <> " " <> logMsgDataToText d
  where
    sysTimeToText :: SystemTime -> Text
    sysTimeToText (MkSystemTime s ns) = "[" <> show s <> ":" <> show ns <> "]"

    timeToText :: SystemTime -> Text
    timeToText sysTime = "[" <> toS (timeToText' (systemToUTCTime sysTime)) <> ":" <> show ms <> "]"
      where
        ms = (systemNanoseconds sysTime) `div` 1000000

    timeToText' = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))

logMsgDataToText :: LogMsgData -> Text
logMsgDataToText LogMsgData{..} =
  "<" <> toS logMsgNodeId <> " | " <> show logMsgNodeState <> ">: " <> logMsg

class Monad m => RaftLogger v m | m -> v where
  loggerCtx :: m (NodeId, RaftNodeState v)

mkLogMsgData :: RaftLogger v m => Text -> m (LogMsgData)
mkLogMsgData msg = do
  (nid, nodeState) <- loggerCtx
  let mode = nodeMode nodeState
  pure $ LogMsgData nid mode msg

instance RaftLogger v m => RaftLogger v (RaftLoggerT v m) where
  loggerCtx = lift loggerCtx

--------------------------------------------------------------------------------
-- Logging with IO
--------------------------------------------------------------------------------

logToDest :: MonadIO m => LogDest -> LogMsg -> m ()
logToDest logDest logMsg =
  case logDest of
    LogStdout -> putText (logMsgToText logMsg)
    LogFile fp -> liftIO $ appendFile fp (logMsgToText logMsg <> "\n")
    NoLogs -> pure ()

logToStdout :: MonadIO m => LogMsg -> m ()
logToStdout = logToDest LogStdout

logToFile :: MonadIO m => FilePath -> LogMsg -> m ()
logToFile fp = logToDest (LogFile fp)

logWithSeverityIO :: forall m v. (RaftLogger v m, MonadIO m) => Severity -> LogDest -> Text -> m ()
logWithSeverityIO s logDest msg = do
  logMsgData <- mkLogMsgData msg
  sysTime <- liftIO getSystemTime
  let logMsg = LogMsg (Just sysTime) s logMsgData
  logToDest logDest logMsg

logInfoIO :: (RaftLogger v m, MonadIO m) => LogDest -> Text -> m ()
logInfoIO = logWithSeverityIO Info

logDebugIO :: (RaftLogger v m, MonadIO m) => LogDest -> Text -> m ()
logDebugIO = logWithSeverityIO Debug

logCriticalIO :: (RaftLogger v m, MonadIO m) => LogDest -> Text -> m ()
logCriticalIO = logWithSeverityIO Critical

--------------------------------------------------------------------------------
-- Pure Logging
--------------------------------------------------------------------------------

newtype RaftLoggerT v m a = RaftLoggerT {
    unRaftLoggerT :: StateT [LogMsg] m a
  } deriving (Functor, Applicative, Monad, MonadState [LogMsg], MonadTrans)

runRaftLoggerT
  :: Monad m
  => RaftLoggerT v m a -- ^ The computation from which to extract the logs
  -> m (a, [LogMsg])
runRaftLoggerT = flip runStateT [] . unRaftLoggerT

type RaftLoggerM v = RaftLoggerT v Identity

runRaftLoggerM
  :: RaftLoggerM v a
  -> (a, [LogMsg])
runRaftLoggerM = runIdentity . runRaftLoggerT

logWithSeverity :: RaftLogger v m => Severity -> Text -> RaftLoggerT v m ()
logWithSeverity s txt = do
  !logMsgData <- mkLogMsgData txt
  let !logMsg = LogMsg Nothing s logMsgData
  modify' (++ [logMsg])

logInfo :: RaftLogger v m => Text -> RaftLoggerT v m ()
logInfo = logWithSeverity Info

logDebug :: RaftLogger v m => Text -> RaftLoggerT v m ()
logDebug = logWithSeverity Debug

logCritical :: RaftLogger v m => Text -> RaftLoggerT v m ()
logCritical = logWithSeverity Critical
