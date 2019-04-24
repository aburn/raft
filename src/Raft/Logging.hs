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
data LogCtx
  = LogCtx
    { logCtxDest :: LogDest
    , logCtxSeverity :: Severity
    }
  | NoLogs

-- | Representation of the logs' destination
data LogDest
  = LogWith (Severity -> Text -> IO ())
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

class Monad m => RaftLogger sm v m | m -> v sm where
  loggerCtx :: m (NodeId, RaftNodeState sm v)

mkLogMsgData :: RaftLogger sm v m => Text -> m (LogMsgData)
mkLogMsgData msg = do
  (nid, nodeState) <- loggerCtx
  let mode = nodeMode nodeState
  pure $ LogMsgData nid mode msg

--------------------------------------------------------------------------------
-- Panic after logging
--------------------------------------------------------------------------------

logAndPanic :: RaftLogger sm v m => Text -> m a
logAndPanic msg = do
  --runRaftLoggerT $ logCritical msg
  panic ("logAndPanic: " <> msg)

logAndPanicIO :: (RaftLogger sm v m, MonadIO m) => LogCtx -> Text -> m a
logAndPanicIO logCtx msg = do
  --logCriticalIO logCtx msg
  panic ("logAndPanicIO: " <> msg)
