module Main (main) where

import Core
import Docker
import RIO
import qualified RIO.Map as Map
import qualified RIO.NonEmpty.Partial as NE
import qualified Runner
import qualified System.Process.Typed as Process
import Test.Hspec

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name
    , image = Image image
    , commands = NE.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps = Pipeline{steps = NE.fromList steps}

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <-
    runner.prepareBuild $
      makePipeline
        [ makeStep "First step" "ubuntu" ["date"]
        , makeStep "Second step" "ubuntu" ["uname -r"]
        ]
  result <- runner.runBuild build
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

main :: IO ()
main = hspec do
  let docker = Docker.createService
  let runner = Runner.createService docker
  beforeAll cleanupDocker $
    describe "Quad CI" do
      it "should run a build (success)" do
        testRunSuccess runner

cleanupDocker :: IO ()
cleanupDocker = void do
  Process.readProcessStdout
    "docker rm -f $(docker ps -aq --filter \"label=quad\")"
