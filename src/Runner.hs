module Runner where

import Control.Concurrent (threadDelay)
import Core
import qualified Docker

data Service = Service
  { prepareBuild :: Pipeline -> IO Build
  , runBuild :: Build -> IO Build
  }

createService :: Docker.Service -> Service
createService docker =
  Service
    { prepareBuild = prepareBuild
    , runBuild = runBuild docker
    }

prepareBuild :: Pipeline -> IO Build
prepareBuild pipeline = do
  return
    Build
      { pipeline = pipeline
      , state = BuildReady
      , completedSteps = mempty
      }

runBuild :: Docker.Service -> Build -> IO Build
runBuild docker build = do
  newBuild <- Core.progress docker build
  case newBuild.state of
    BuildFinished _ -> return newBuild
    _ -> do
      threadDelay (1 * 1000 * 1000)
      runBuild docker newBuild
