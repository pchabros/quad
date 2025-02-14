module Core where

import qualified Docker
import RIO
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE

newtype Pipeline = Pipeline {steps :: NonEmpty Step}
  deriving (Eq, Show)

data Step = Step
  { name :: StepName
  , commands :: NonEmpty Text
  , image :: Docker.Image
  }
  deriving (Eq, Show)

newtype StepName = StepName Text
  deriving (Eq, Show, Ord)

data StepResult = StepFailed Docker.ContainerExitCode | StepSucceeded
  deriving (Eq, Show)

-- NOTE: Why not record syntax (getter for free)?
stepNameToText :: StepName -> Text
stepNameToText (StepName text) = text

imageToText :: Docker.Image -> Text
imageToText (Docker.Image text) = text

type CompletedSteps = Map StepName StepResult

data Build = Build
  { pipeline :: Pipeline
  , state :: BuildState
  , completedSteps :: CompletedSteps
  }
  deriving (Eq, Show)

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState -- NOTE: Why not just Step.name?
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildRunningState = BuildRunningState
  { step :: StepName
  , container :: Docker.ContainerId
  }
  deriving (Eq, Show)

data BuildResult = BuildSucceeded | BuildFailed | BuildUnexpectedState Text
  deriving (Eq, Show)

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
        Right step -> do
          let options = Docker.CreateContainerOptions{image = step.image}
          container <- docker.createContainer options
          docker.startContainer container
          let state = BuildRunning BuildRunningState{step = step.name, container}
          return build{state}
    BuildRunning state -> do
      status <- docker.containerStatus state.container
      case status of
        Docker.ContainerRunning -> return build
        Docker.ContainerExited exitCode ->
          let
            result = exitCodeToStepResult exitCode
            completedSteps = Map.insert state.step result build.completedSteps
           in
            return $ build{state = BuildReady, completedSteps}
        Docker.ContainerOther other ->
          return build{state = BuildFinished $ BuildUnexpectedState other}
    BuildFinished _ ->
      return build
