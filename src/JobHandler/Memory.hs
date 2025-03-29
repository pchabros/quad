module JobHandler.Memory where

import qualified Agent
import qualified Control.Concurrent.STM as STM
import Core
import qualified JobHandler
import RIO hiding (log)
import qualified RIO.List as List
import qualified RIO.Map as Map

data State = State
  { jobs :: Map BuildNumber JobHandler.Job
  , logs :: Map (BuildNumber, StepName) ByteString
  , nextBuild :: Int
  }
  deriving (Eq, Show)

queueJob :: Pipeline -> State -> (BuildNumber, State)
queueJob pipeline state = (number, updatedState)
 where
  number = BuildNumber state.nextBuild
  job = JobHandler.Job{pipeline, state = JobHandler.JobQueued}
  updatedState =
    state
      { jobs = Map.insert number job state.jobs
      , nextBuild = state.nextBuild + 1
      }

findJob :: BuildNumber -> State -> Maybe JobHandler.Job
findJob number state = Map.lookup number state.jobs

dispatchCmd :: State -> (Maybe Agent.Cmd, State)
dispatchCmd state =
  case List.find queued (Map.toList state.jobs) of
    Just (number, job) -> (Just cmd, updatedState)
     where
      updatedJob = job{JobHandler.state = JobHandler.JobAssigned}
      updatedJobs = Map.insert number updatedJob state.jobs
      cmd = Agent.StartBuild number job.pipeline
      updatedState = state{jobs = updatedJobs}
    _ -> (Nothing, state)
 where
  queued :: (BuildNumber, JobHandler.Job) -> Bool
  queued (_, job) = job.state == JobHandler.JobQueued

processMsg :: Agent.Msg -> State -> State
processMsg msg state = case msg of
  Agent.BuildUpdated number build ->
    state{jobs = Map.adjust schedule number state.jobs}
   where
    schedule job = job{JobHandler.state = JobHandler.JobScheduled build}
  Agent.LogCollected number log ->
    state{logs = updatedLogs}
   where
    updatedLogs = Map.insertWith (flip mappend) (number, log.step) log.output state.logs

createService :: IO JobHandler.Service
createService = do
  state <-
    STM.newTVarIO
      State
        { jobs = mempty
        , logs = mempty
        , nextBuild = 1
        }
  pure
    JobHandler.Service
      { queueJob = \pipeline -> STM.atomically do
          STM.stateTVar state $ queueJob pipeline
      , findJob = \number -> STM.atomically do
          findJob number <$> STM.readTVar state
      , dispatchCmd = STM.atomically do
          STM.stateTVar state dispatchCmd
      , processMsg = \msg -> STM.atomically do
          STM.modifyTVar' state $ processMsg msg
      }
