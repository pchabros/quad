module Main (main) where

import qualified Agent
import qualified Control.Concurrent.Async as Async
import Core
import qualified Data.Yaml as Yaml
import Docker
import qualified JobHandler
import qualified JobHandler.Memory
import RIO hiding (log)
import qualified RIO.ByteString as ByteString
import qualified RIO.Map as Map
import qualified RIO.NonEmpty.Partial as NE
import qualified RIO.Set as Set
import qualified Runner
import qualified Server
import qualified System.Process.Typed as Process
import Test.Hspec
import Prelude hiding (log)

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name
    , image = Image image "latest"
    , commands = NE.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps = Pipeline{steps = NE.fromList steps}

emptyHooks :: Runner.Hooks
emptyHooks =
  Runner.Hooks
    { logCollected = \_ -> pure ()
    , buildUpdated = \_ -> pure ()
    }

checkBuild :: JobHandler.Service -> BuildNumber -> IO ()
checkBuild service number = loop
 where
  loop = do
    (Just job) <- service.findJob number
    case job.state of
      (JobHandler.JobScheduled build) -> do
        case build.state of
          BuildFinished state -> state `shouldBe` BuildSucceeded
          _ -> loop
      _ -> loop

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner =
  runner.prepareBuild
    ( makePipeline
        [ makeStep "First step" "ubuntu" ["date"]
        , makeStep "Second step" "ubuntu" ["uname -r"]
        ]
    )
    >>= runner.runBuild emptyHooks
    >>= \result -> do
      result.state `shouldBe` BuildFinished BuildSucceeded
      Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner =
  runner.prepareBuild
    (makePipeline [makeStep "Should fail" "ubuntu" ["exit 1"]])
    >>= runner.runBuild emptyHooks
    >>= \result -> do
      result.state `shouldBe` BuildFinished BuildFailed
      Map.elems result.completedSteps
        `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

testSharedVolume :: Runner.Service -> IO ()
testSharedVolume runner =
  runner.prepareBuild
    ( makePipeline
        [ makeStep "Create file" "ubuntu" ["echo hello > test"]
        , makeStep "Read file" "ubuntu" ["cat test"]
        ]
    )
    >>= runner.runBuild emptyHooks
    >>= \result -> do
      result.state `shouldBe` BuildFinished BuildSucceeded
      Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testLogCollection :: Runner.Service -> IO ()
testLogCollection runner = do
  expected <- newMVar $ Set.fromList ["hello", "world", "Linux" :: ByteString]
  let onLog :: Log -> IO ()
      onLog log = do
        remaining <- readMVar expected
        forM_ remaining $ \word -> do
          case ByteString.breakSubstring word log.output of
            (_, "") -> pure ()
            _ -> modifyMVar_ expected $ pure . Set.delete word
  let hooks = Runner.Hooks{logCollected = onLog, buildUpdated = \_ -> pure ()}
  build <-
    runner.prepareBuild $
      makePipeline
        [ makeStep "Long step" "ubuntu" ["echo hello", "sleep 2", "echo world"]
        , makeStep "Echo Linux" "ubuntu" ["uname -s"]
        ]
  result <- runner.runBuild hooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]
  readMVar expected >>= (`shouldBe` Set.empty)

testImagePull :: Runner.Service -> IO ()
testImagePull runner =
  Process.readProcessStdout "docker rmi -f busybox"
    >> ( runner.prepareBuild $
          makePipeline [makeStep "First step" "busybox" ["date"]]
       )
    >>= runner.runBuild emptyHooks
    >>= \result -> do
      result.state `shouldBe` BuildFinished BuildSucceeded
      Map.elems result.completedSteps `shouldBe` [StepSucceeded]

testYamlDecoding :: Runner.Service -> IO ()
testYamlDecoding runner = do
  pipeline <- Yaml.decodeFileThrow "test/pipeline.sample.yml"
  build <- runner.prepareBuild pipeline
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded

testServerAgent :: Runner.Service -> IO ()
testServerAgent runner = do
  handler <- JobHandler.Memory.createService

  serverThread <- Async.async (Server.run (Server.Config 9000) handler)
  Async.link serverThread

  agentThread <-
    Async.async (Agent.run (Agent.Config "http://localhost:9000") runner)
  Async.link agentThread

  handler.queueJob pipeline >>= checkBuild handler

  traverse_ Async.cancel [serverThread, agentThread]
 where
  pipeline =
    makePipeline
      [makeStep "agent-test" "busybox" ["echo hello", "echo from agent"]]

main :: IO ()
main = hspec do
  docker <- runIO Docker.createService
  let runner = Runner.createService docker
  beforeAll cleanupDocker $
    describe "Quad CI" do
      it "should run a build (success)" do
        testRunSuccess runner
      it "should run a build (failure)" do
        testRunFailure runner
      it "should share volume between steps" do
        testSharedVolume runner
      it "should collect logs" do
        testLogCollection runner
      it "should pull image" do
        testImagePull runner
      it "should decode pipelines" do
        testYamlDecoding runner
      it "should run server and agent" do
        testServerAgent runner

cleanupDocker :: IO ()
cleanupDocker =
  void $
    Process.readProcessStdout
      "docker rm -f $(docker ps -aq --filter \"label=quad\")"
      >> Process.readProcessStdout
        "docker volume rm -f $(docker volume ls -q --filter \"label=quad\")"
