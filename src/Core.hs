module Core where

import qualified Data.Time.Clock.POSIX as Time
import qualified Docker
import RIO
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as Text

newtype Pipeline = Pipeline {steps :: Steps}
  deriving (Eq, Show)

data Step = Step
  { name :: StepName
  , commands :: Commands
  , image :: Docker.Image
  }
  deriving (Eq, Show)

type Steps = NonEmpty Step

type Commands = NonEmpty Text

newtype StepName = StepName Text
  deriving (Eq, Show, Ord)

data StepResult = StepFailed Docker.ContainerExitCode | StepSucceeded
  deriving (Eq, Show)

type CompletedSteps = Map StepName StepResult

data Build = Build
  { pipeline :: Pipeline
  , state :: BuildState
  , completedSteps :: CompletedSteps
  , volume :: Docker.Volume
  }
  deriving (Eq, Show)

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildRunningState = BuildRunningState
  { step :: StepName
  , container :: Docker.ContainerId
  }
  deriving (Eq, Show)

data BuildResult = BuildSucceeded | BuildFailed | BuildUnexpectedState Text
  deriving (Eq, Show)

type LogCollection = Map StepName CollectionStatus

data CollectionStatus
  = CollectionReady
  | CollectingLogs Docker.ContainerId Time.POSIXTime
  | CollectionFinished
  deriving (Eq, Show)

data Log = Log
  { output :: ByteString
  , step :: StepName
  }
  deriving (Eq, Show)

commandsToScript :: Commands -> Docker.Script
commandsToScript commands = Docker.Script $ Text.unlines $ ["set -ex"] <> NE.toList commands

buildResult :: CompletedSteps -> BuildResult
buildResult steps
  | all (== StepSucceeded) steps = BuildSucceeded
  | otherwise = BuildFailed

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep Build{pipeline, completedSteps}
  | BuildFailed <- result = Left result
  | (step : _) <- NE.filter notComplited pipeline.steps = Right step
  | otherwise = Left result
 where
  result = buildResult completedSteps
  notComplited Step{name} = name `notElem` Map.keys completedSteps

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult (Docker.ContainerExitCode n)
  | n == 0 = StepSucceeded
  | otherwise = StepFailed $ Docker.ContainerExitCode n

progress :: Docker.Service -> Build -> IO Build
progress docker build =
  case build.state of
    BuildReady ->
      case buildHasNextStep build of
        Left result ->
          return $ build{state = BuildFinished result}
        Right step ->
          docker.pullImage step.image
            >> docker.createContainer options
            >>= docker.startContainer <&> updateBuild
         where
          options =
            Docker.CreateContainerOptions
              { image = step.image
              , script = commandsToScript step.commands
              , volume = build.volume
              }
          updateBuild container =
            build{state = BuildRunning BuildRunningState{step = step.name, container}}
    BuildRunning state ->
      docker.containerStatus state.container >>= \case
        Docker.ContainerRunning -> return build
        Docker.ContainerExited exitCode ->
          return $ build{state = BuildReady, completedSteps}
         where
          completedSteps = Map.insert state.step result build.completedSteps
          result = exitCodeToStepResult exitCode
        Docker.ContainerOther other ->
          return build{state = BuildFinished $ BuildUnexpectedState other}
    BuildFinished _ ->
      return build

initLogCollection :: Steps -> LogCollection
initLogCollection = Map.fromList . NE.toList . initSteps
 where
  initSteps = fmap \step -> (step.name, CollectionReady)

runCollection :: Docker.Service -> Time.POSIXTime -> LogCollection -> IO [Log]
runCollection docker collectUntil collection = concat . Map.elems <$> Map.traverseWithKey collect collection
 where
  collect step = \case
    CollectionReady -> return []
    CollectionFinished -> return []
    CollectingLogs container since ->
      docker.fetchLogs options
        >>= \output -> return [Log{step, output}]
     where
      options = Docker.FetchLogsOptions{container, since, until = collectUntil}

updateCollection ::
  BuildState -> Time.POSIXTime -> LogCollection -> LogCollection
updateCollection state now = Map.mapWithKey f
 where
  f step = \case
    CollectionReady -> update step 0 CollectionReady
    CollectingLogs _ _ -> update step now CollectionFinished
    CollectionFinished -> CollectionFinished

  update step since nextState = case state of
    BuildRunning runningState ->
      if runningState.step == step
        then CollectingLogs runningState.container since
        else nextState
    _ -> nextState

collectLogs ::
  Docker.Service -> LogCollection -> Build -> IO (LogCollection, [Log])
collectLogs docker collection build = do
  now <- Time.getPOSIXTime
  logs <- runCollection docker now collection
  let newCollection = updateCollection build.state now collection
  return (newCollection, logs)
