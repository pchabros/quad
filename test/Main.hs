module Main (main) where

import Core
import Docker
import RIO
import qualified RIO.NonEmpty.Partial as NE

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands =
  Step
    { name = StepName name
    , image = Image image
    , commands = NE.fromList commands
    }

makePipeline :: [Step] -> Pipeline
makePipeline steps = Pipeline{steps = NE.fromList steps}

-- Arrange
testPipeline :: Pipeline
testPipeline =
  makePipeline
    [ makeStep "First step" "ubuntu" ["date"]
    , makeStep "Second step" "ubuntu" ["uname -r"]
    ]

testBuild :: Build
testBuild =
  Build
    { pipeline = testPipeline
    , state = BuildReady
    , completedSteps = mempty
    }

main :: IO ()
main = pure ()
