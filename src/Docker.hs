module Docker where

import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Socket

newtype Image = Image {name :: Text}
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

newtype CreateContainerOptions = CreateContainerOptions {image :: Image}

createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
  manager <- Socket.newManager "/var/run/docker.sock"
  let body = Aeson.object [("Image", Aeson.toJSON options.image.name)]
  let req =
        HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestPath "/v1.47/containers/create"
          & HTTP.setRequestBodyJSON body
  res <- HTTP.httpBS req
  traceShowIO res
