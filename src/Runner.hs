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
    { prepareBuild = prepareBuild docker
    , runBuild = runBuild docker
    }

prepareBuild :: Docker.Service -> Pipeline -> IO Build
prepareBuild docker pipeline =
  docker.createVolume >>= \volume ->
    return
      Build
        { pipeline = pipeline
        , state = BuildReady
        , completedSteps = mempty
        , volume
        }

runBuild :: Docker.Service -> Build -> IO Build
runBuild docker build = do
  newBuild <- Core.progress docker build
  case newBuild.state of
    BuildFinished _ -> return newBuild
    _ -> do
      threadDelay (1 * 1000 * 1000)
      runBuild docker newBuild
