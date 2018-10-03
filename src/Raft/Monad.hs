{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Raft.Monad where

import Protolude
import qualified Debug.Trace as DT
import Control.Monad.RWS
import qualified Data.Set as Set
import qualified Data.Map as Map

import Raft.Types

--------------------------------------------------------------------------------
-- Raft Monad
--------------------------------------------------------------------------------

newtype TransitionM v a = TransitionM
  { unTransitionM :: RWS NodeConfig [Action v] (PersistentState v) a
  } deriving (Functor, Applicative, Monad, MonadWriter [Action v], MonadReader NodeConfig, MonadState (PersistentState v))

runTransitionM
  :: NodeConfig
  -> PersistentState v
  -> TransitionM v a
  -> (a, PersistentState v, [Action v])
runTransitionM nodeConfig persistentState transition =
  runRWS (unTransitionM transition) nodeConfig persistentState

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

type RPCHandler s r v = RPCType r v => NodeState s -> NodeId -> r -> TransitionM v (ResultState s v)
type TimeoutHandler s v = NodeState s -> Timeout -> TransitionM v (ResultState s v)
type ClientReqHandler s v = NodeState s -> ClientReq v -> TransitionM v (ResultState s v)

--------------------------------------------------------------------------------
-- Transitions
--------------------------------------------------------------------------------

data Mode
  = Follower
  | Candidate
  | Leader
  deriving (Show)

-- | All valid state transitions of a Raft node
data Transition (init :: Mode) (res :: Mode) where
  StartElection            :: Transition 'Follower 'Candidate
  HigherTermFoundFollower  :: Transition 'Follower 'Follower

  RestartElection          :: Transition 'Candidate 'Candidate
  DiscoverLeader           :: Transition 'Candidate 'Follower
  HigherTermFoundCandidate :: Transition 'Candidate 'Follower
  BecomeLeader             :: Transition 'Candidate 'Leader

  SendHeartbeat            :: Transition 'Leader 'Leader
  DiscoverNewLeader        :: Transition 'Leader 'Follower
  HigherTermFoundLeader    :: Transition 'Leader 'Follower

  -- TODO Replace with specific transition names
  Noop :: Transition init init

deriving instance Show (Transition init res)

-- | The volatile state of a Raft Node
data NodeState (a :: Mode) where
  NodeFollowerState :: FollowerState -> NodeState 'Follower
  NodeCandidateState :: CandidateState -> NodeState 'Candidate
  NodeLeaderState :: LeaderState -> NodeState 'Leader

deriving instance Show (NodeState v)

-- | Existential type hiding the result type of a transition
data ResultState init v where
  ResultState :: Transition init res -> NodeState res -> ResultState init v

deriving instance Show (ResultState init v)

followerResultState
  :: Transition init 'Follower
  -> FollowerState
  -> ResultState init v
followerResultState transition fstate =
  ResultState transition (NodeFollowerState fstate)

candidateResultState
  :: Transition init 'Candidate
  -> CandidateState
  -> ResultState init v
candidateResultState transition cstate =
  ResultState transition (NodeCandidateState cstate)

leaderResultState
  :: Transition init 'Leader
  -> LeaderState
  -> ResultState init v
leaderResultState transition lstate =
  ResultState transition (NodeLeaderState lstate)

-- | Existential type hiding the internal node state
data RaftNodeState v where
  RaftNodeState :: NodeState s -> RaftNodeState v

deriving instance Show (RaftNodeState v)

--------------------------------------------------------------------------------
-- DSL (TODO move to src/Raft/Action.hs)
--------------------------------------------------------------------------------

-- | Helper for message actions
toRPCMessage :: RPCType r v => r -> TransitionM v (Message v)
toRPCMessage msg = flip RPC (toRPC msg) <$> asks configNodeId

broadcast :: RPCType r v => r -> TransitionM v ()
broadcast msg = do
  selfNodeId <- asks configNodeId
  action <-
    Broadcast
      <$> asks (Set.filter (selfNodeId /=) . configNodeIds)
      <*> toRPCMessage msg
  tell [action]

send :: RPCType r v => NodeId -> r -> TransitionM v ()
send nodeId msg = do
  action <- SendMessage nodeId <$> toRPCMessage msg
  tell [action]

uniqueBroadcast :: RPCType r v => Map NodeId r -> TransitionM v ()
uniqueBroadcast msgs = do
  action <- SendMessages <$> mapM toRPCMessage msgs
  tell [action]

incrementTerm :: TransitionM v ()
incrementTerm = do
  psNextTerm <- gets (incrTerm . psCurrentTerm)
  modify $ \pstate ->
    pstate { psCurrentTerm = psNextTerm }

-- | Resets the election timeout.
resetElectionTimeout :: TransitionM v ()
resetElectionTimeout = do
  t <- asks configElectionTimeout
  tell [ResetTimeoutTimer ElectionTimeout t]

resetHeartbeatTimeout :: TransitionM v ()
resetHeartbeatTimeout = do
  t <- asks configHeartbeatTimeout
  tell [ResetTimeoutTimer HeartbeatTimeout t]

applyLogEntry :: Index -> TransitionM v ()
applyLogEntry idx = do
  mLogEntry <- lookupLogEntry idx <$> gets psLog
  case mLogEntry of
    Nothing -> panic "Cannot apply non existent log entry to state machine"
    Just logEntry -> tell [ApplyCommittedEntry logEntry]

appendNewLogEntries :: Seq (Entry v) -> TransitionM v ()
appendNewLogEntries newEntries =
  modify $ \pstate ->
    case appendLogEntries (psLog pstate) newEntries of
      Left err -> panic (show err)
      Right newLog -> pstate { psLog = newLog }

updateElectionTimeoutCandidateState :: Index -> Index -> TransitionM v CandidateState
updateElectionTimeoutCandidateState commitIndex lastApplied = do
  -- State modifications
  incrementTerm
  voteForSelf
  -- Actions to perform
  resetElectionTimeout
  broadcast =<< requestVoteMessage
  selfNodeId <- asks configNodeId

  -- Return new candidate state
  pure CandidateState
    { csCommitIndex = commitIndex
    , csLastApplied = lastApplied
    , csVotes = Set.singleton selfNodeId
    }
  where
  requestVoteMessage = do
    term <- gets psCurrentTerm
    selfNodeId <- asks configNodeId
    (logEntryIndex, logEntryTerm) <-
      lastLogEntryIndexAndTerm <$> gets psLog
    pure RequestVote
      { rvTerm = term
      , rvCandidateId = selfNodeId
      , rvLastLogIndex = logEntryIndex
      , rvLastLogTerm = logEntryTerm
      }

  voteForSelf = do
    selfNodeId <- asks configNodeId
    modify $ \pstate ->
      pstate { psVotedFor = Just selfNodeId }

isLeader :: RaftNodeState v -> Bool
isLeader (RaftNodeState (NodeFollowerState _)) = False
isLeader (RaftNodeState (NodeCandidateState _)) = False
isLeader (RaftNodeState (NodeLeaderState _)) = True

isCandidate :: RaftNodeState v -> Bool
isCandidate (RaftNodeState (NodeFollowerState _)) = False
isCandidate (RaftNodeState (NodeCandidateState _)) = True
isCandidate (RaftNodeState (NodeLeaderState _)) = False

isFollower :: RaftNodeState v -> Bool
isFollower (RaftNodeState (NodeFollowerState _)) = True
isFollower (RaftNodeState (NodeCandidateState _)) = False
isFollower (RaftNodeState (NodeLeaderState _)) = False
