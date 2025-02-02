module Core where

import Docker
import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NE

newtype Pipeline = Pipeline {steps :: NonEmpty Step}
  deriving (Eq, Show)

data Step = Step
  { name :: StepName
  , commands :: NonEmpty Text
  , image :: Image
  }
  deriving (Eq, Show)

newtype StepName = StepName Text
  deriving (Eq, Show, Ord)
data StepResult = StepFailed ContainerExitCode | StepSucceeded
  deriving (Eq, Show)

-- NOTE: Why not record syntax (getter for free)?
stepNameToText :: StepName -> Text
stepNameToText (StepName text) = text

imageToText :: Image -> Text
imageToText (Image text) = text

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

newtype BuildRunningState = BuildRunningState {step :: StepName}
  deriving (Eq, Show)
data BuildResult = BuildSucceeded | BuildFailed
  deriving (Eq, Show)

buildResult :: CompletedSteps -> BuildResult
buildResult steps
  | List.all (== StepSucceeded) steps = BuildSucceeded
  | otherwise = BuildFailed

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep Build{pipeline, completedSteps}
  | BuildFailed <- result = Left result
  | (step : _) <- NE.filter notComplited pipeline.steps = Right step
  | otherwise = Left result
 where
  result = buildResult completedSteps
  notComplited Step{name} = name `notElem` Map.keys completedSteps

exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult (ContainerExitCode n)
  | n == 0 = StepSucceeded
  | otherwise = StepFailed $ ContainerExitCode n

progress :: Build -> IO Build
progress build =
  case build.state of
    BuildReady ->
      case buildHasNextStep build of
        Left result ->
          pure $ build{state = BuildFinished result}
        Right step ->
          pure $ build{state = BuildRunning (BuildRunningState step.name)}
    BuildRunning state ->
      let result = exitCodeToStepResult $ ContainerExitCode 0 -- NOTE: For now assuming success
          completedSteps' = Map.insert state.step result build.completedSteps
       in pure $ build{state = BuildReady, completedSteps = completedSteps'}
    BuildFinished _ ->
      pure build
