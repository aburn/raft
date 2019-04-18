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
import Control.Monad.State.Strict (modify')

import Data.Time
import Data.Time.Clock.System

import Raft.NodeState
import Raft.Types

-- | Representation of the logs' context
data LogCtx m
  = LogCtx
    { logCtxDest :: LogDest m
    , logCtxSeverity :: Severity
    }
  | NoLogs

-- | Representation of the logs' destination
data LogDest m
  = LogWith (MonadIO m => Severity -> Text -> m ())
  | LogFile FilePath
  | LogStdout

-- | Representation of the severity of the logs
data Severity
  = Debug
  | Info
  | Critical
  deriving (Show, Eq, Ord)

data LogMsg = LogMsg
  { mTime :: Maybe SystemTime
  , severity :: Severity
  , logMsgData :: LogMsgData
  } deriving (Show)

data LogMsgData = LogMsgData
  { logMsgNodeId :: NodeId
  , logMsgNodeState :: Mode
  , logMsg :: Text
  } deriving (Show)

logMsgToText :: LogMsg -> Text
logMsgToText (LogMsg mt s d) =
    maybe "" timeToText mt <> "(" <> show s <> ")" <> " " <> logMsgDataToText d
  where
    timeToText :: SystemTime -> Text
    timeToText sysTime = "[" <> toS (timeToText' (systemToUTCTime sysTime)) <> ":" <> show ms <> "]"
      where
        ms = (systemNanoseconds sysTime) `div` 1000000

    timeToText' = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))

logMsgDataToText :: LogMsgData -> Text
logMsgDataToText LogMsgData{..} =
  "<" <> toS logMsgNodeId <> " | " <> show logMsgNodeState <> ">: " <> logMsg

class Monad m => RaftLogger sm v m | m -> v sm where
  loggerCtx :: m (NodeId, RaftNodeState sm v)

mkLogMsgData :: RaftLogger sm v m => Text -> m (LogMsgData)
mkLogMsgData msg = do
  (nid, nodeState) <- loggerCtx
  let mode = nodeMode nodeState
  pure $ LogMsgData nid mode msg

instance RaftLogger sm v m => RaftLogger sm v (RaftLoggerT sm v m) where
  loggerCtx = lift loggerCtx
--------------------------------------------------------------------------------
-- Pure Logging
--------------------------------------------------------------------------------

newtype RaftLoggerT sm v m a = RaftLoggerT {
    unRaftLoggerT :: StateT [LogMsg] m a
  } deriving (Functor, Applicative, Monad, MonadState [LogMsg], MonadTrans, MonadIO)

runRaftLoggerT
  :: Monad m
  => RaftLoggerT sm v m a -- ^ The computation from which to extract the logs
  -> m (a, [LogMsg])
runRaftLoggerT = flip runStateT [] . unRaftLoggerT

type RaftLoggerM sm v = RaftLoggerT sm v Identity

runRaftLoggerM
  :: RaftLoggerM sm v a
  -> (a, [LogMsg])
runRaftLoggerM = runIdentity . runRaftLoggerT

logWithSeverity :: RaftLogger sm v m => Severity -> Text -> RaftLoggerT sm v m ()
logWithSeverity s txt = do
  !logMsgData <- mkLogMsgData txt
  let !logMsg = LogMsg Nothing s logMsgData
  modify' (++ [logMsg])

logInfo :: RaftLogger sm v m => Text -> RaftLoggerT sm v m ()
logInfo = logWithSeverity Info

logDebug :: RaftLogger sm v m => Text -> RaftLoggerT sm v m ()
logDebug = logWithSeverity Debug

logCritical :: RaftLogger sm v m => Text -> RaftLoggerT sm v m ()
logCritical = logWithSeverity Critical

--------------------------------------------------------------------------------
-- Panic after logging
--------------------------------------------------------------------------------

logAndPanic :: RaftLogger sm v m => Text -> m a
logAndPanic msg = do
  runRaftLoggerT $ logCritical msg
  panic ("logAndPanic: " <> msg)

logAndPanicIO :: (RaftLogger sm v m, MonadIO m) => LogCtx m -> Text -> m a
logAndPanicIO logCtx msg = do
  --logCriticalIO logCtx msg
  panic ("logAndPanicIO: " <> msg)
