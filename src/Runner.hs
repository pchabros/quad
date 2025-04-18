module Runner where

import Core
import qualified Docker
import RIO

data Hooks = Hooks
  { logCollected :: Log -> IO ()
  , buildUpdated :: Build -> IO ()
  }

data Service = Service
  { prepareBuild :: Pipeline -> IO Build
  , runBuild :: Hooks -> Build -> IO Build
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

runBuild :: Docker.Service -> Hooks -> Build -> IO Build
runBuild docker hooks build = do
  loop build $ Core.initLogCollection build.pipeline.steps
 where
  loop :: Build -> LogCollection -> IO Build
  loop build' collection = do
    (newCollection, logs) <- Core.collectLogs docker collection build'
    traverse_ hooks.logCollected logs
    newBuild <- Core.progress docker build'
    hooks.buildUpdated newBuild
    case newBuild.state of
      BuildFinished _ -> return newBuild
      _ -> do
        threadDelay (1 * 1000 * 1000)
        loop newBuild newCollection
