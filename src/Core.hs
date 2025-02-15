module Core where

import qualified Docker
import RIO
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as Text

newtype Pipeline = Pipeline {steps :: NonEmpty Step}
  deriving (Eq, Show)

data Step = Step
  { name :: StepName
  , commands :: Commands
  , image :: Docker.Image
  }
  deriving (Eq, Show)

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

parseCommands :: Commands -> Docker.Script
parseCommands commands = Docker.Script $ Text.unlines $ ["set -ex"] <> NE.toList commands

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
          docker.createContainer options >>= docker.startContainer >>= updatedBuild
         where
          options =
            Docker.CreateContainerOptions
              { image = step.image
              , script = parseCommands step.commands
              }
          updatedBuild container =
            return
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
