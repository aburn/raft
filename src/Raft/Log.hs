{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Raft.Log where

import Protolude

import Data.Serialize
import Data.Sequence (Seq(..), (|>))

import Raft.Types

data EntryIssuer
  = ClientIssuer ClientId
  | LeaderIssuer LeaderId
  deriving (Show, Generic, Serialize)

data EntryValue v
  = EntryValue v
  | NoValue -- ^ Used as a first committed entry of a new term
  deriving (Show, Generic, Serialize)

-- | Representation of an entry in the replicated log
data Entry v = Entry
  { entryIndex :: Index
    -- ^ Index of entry in the log
  , entryTerm :: Term
    -- ^ Term when entry was received by leader
  , entryValue :: EntryValue v
    -- ^ Command to update state machine
  , entryIssuer :: EntryIssuer
    -- ^ Id of the client that issued the command
  } deriving (Show, Generic, Serialize)

type Entries v = Seq (Entry v)

data InvalidLog
  = InvalidIndex { expected :: Index, actual :: Index }
  deriving (Show)

-- | For debugging purposes
validateLog :: Entries v -> Either InvalidLog ()
validateLog es =
    case traverse (uncurry validateIndex) (zip (toList es) [Index 1..]) of
      Left e -> Left e
      Right _ -> Right ()
  where
    validateIndex e idx
      | entryIndex e == idx = Right ()
      | otherwise = Left (InvalidIndex idx (entryIndex e))

-- | Provides an interface for nodes to write log entries to storage.
class (Show (RaftWriteLogError m), Monad m) => RaftWriteLog m v where
  type RaftWriteLogError m
  -- | Write the given log entries to storage
  writeLogEntries
    :: Exception (RaftWriteLogError m)
    => Entries v -> m (Either (RaftWriteLogError m) ())

data DeleteSuccess v = DeleteSuccess

-- | Provides an interface for nodes to delete log entries from storage.
class (Show (RaftDeleteLogError m), Monad m) => RaftDeleteLog m v where
  type RaftDeleteLogError m
  -- | Delete log entries from a given index; e.g. 'deleteLogEntriesFrom 7'
  -- should delete every log entry with an index >= 7.
  deleteLogEntriesFrom
    :: Exception (RaftDeleteLogError m)
    => Index -> m (Either (RaftDeleteLogError m) (DeleteSuccess v))

-- | Provides an interface for nodes to read log entries from storage.
class (Show (RaftReadLogError m), Monad m) => RaftReadLog m v where
  type RaftReadLogError m
  -- | Read the log at a given index
  readLogEntry
    :: Exception (RaftReadLogError m)
    => Index -> m (Either (RaftReadLogError m) (Maybe (Entry v)))
  -- | Read log entries from a specific index onwards, including the specific
  -- index
  readLogEntriesFrom
    :: Exception (RaftReadLogError m)
    => Index -> m (Either (RaftReadLogError m) (Entries v))
  -- | Read the last log entry in the log
  readLastLogEntry
    :: Exception (RaftReadLogError m)
    => m (Either (RaftReadLogError m) (Maybe (Entry v)))

  default readLogEntriesFrom
    :: Exception (RaftReadLogError m)
    => Index
    -> m (Either (RaftReadLogError m) (Entries v))
  readLogEntriesFrom idx = do
      eLastLogEntry <- readLastLogEntry
      case eLastLogEntry of
        Left err -> pure (Left err)
        Right Nothing -> pure (Right Empty)
        Right (Just lastLogEntry)
          | entryIndex lastLogEntry < idx -> pure (Right Empty)
          | otherwise -> fmap (|> lastLogEntry) <$> go (decrIndexWithDefault0 (entryIndex lastLogEntry))
    where
      go idx'
        | idx' < idx || idx' == 0 = pure (Right Empty)
        | otherwise = do
            eLogEntry <- readLogEntry idx'
            case eLogEntry of
              Left err -> pure (Left err)
              Right Nothing -> panic "Malformed log"
              Right (Just logEntry) -> fmap (|> logEntry) <$>  go (decrIndexWithDefault0 idx')

type RaftLog m v = (RaftReadLog m v, RaftWriteLog m v, RaftDeleteLog m v)
type RaftLogExceptions m = (Exception (RaftReadLogError m), Exception (RaftWriteLogError m), Exception (RaftDeleteLogError m))

-- | Representation of possible errors that come from reading, writing or
-- deleting logs from the persistent storage
data RaftLogError m where
  RaftLogReadError :: Show (RaftReadLogError m) => RaftReadLogError m -> RaftLogError m
  RaftLogWriteError :: Show (RaftWriteLogError m) => RaftWriteLogError m -> RaftLogError m
  RaftLogDeleteError :: Show (RaftDeleteLogError m) => RaftDeleteLogError m -> RaftLogError m

deriving instance Show (RaftLogError m)

updateLog
  :: forall m v.
     ( RaftDeleteLog m v, Exception (RaftDeleteLogError m)
     , RaftWriteLog m v, Exception (RaftWriteLogError m)
     )
  => Entries v
  -> m (Either (RaftLogError m) ())
updateLog entries =
  case entries of
    Empty -> pure (Right ())
    e :<| _ -> do
      eDel <- deleteLogEntriesFrom @m @v (entryIndex e)
      case eDel of
        Left err -> pure (Left (RaftLogDeleteError err))
        Right DeleteSuccess -> first RaftLogWriteError <$> writeLogEntries entries
