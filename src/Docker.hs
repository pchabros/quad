module Docker where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Socket

newtype Image = Image {name :: Text}
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

newtype ContainerId = ContainerId Text
  deriving (Eq, Show, Aeson.FromJSON)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

newtype CreateContainerOptions = CreateContainerOptions {image :: Image}

data ResponseBody = ResponseBody
  { id :: ContainerId
  , warnings :: [String]
  }
  deriving (Show)

instance Aeson.FromJSON ResponseBody where
  parseJSON =
    Aeson.withObject
      "ResponseBody"
      $ \obj -> do
        _id <- obj .: "Id"
        _warnings <- obj .: "Warnings"
        return (ResponseBody _id _warnings)

parseResponse :: (Aeson.FromJSON a) => HTTP.Response ByteString -> a
parseResponse res = either error id $ Aeson.eitherDecodeStrict $ HTTP.getResponseBody res

createContainer :: CreateContainerOptions -> IO ContainerId
createContainer options = do
  manager <- Socket.newManager "/var/run/docker.sock"
  let body =
        Aeson.object
          [ ("Image", Aeson.toJSON options.image.name)
          , ("Tty", Aeson.toJSON True)
          , ("Labels", Aeson.object [("quad", "")])
          , ("Cmd", "echo hello")
          , ("Entrypoint", Aeson.toJSON ["/bin/sh" :: String, "-c"])
          ]
  let req =
        HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestPath "/v1.47/containers/create"
          & HTTP.setRequestBodyJSON body
  res <- HTTP.httpBS req
  let ResponseBody{id = _id} = parseResponse res
  return _id

startContainer :: ContainerId -> IO ()
startContainer (ContainerId _id) = do
  manager <- Socket.newManager "/var/run/docker.sock"
  let path = "/v1.47/containers/" <> _id <> "/start"
  let req =
        HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestPath (encodeUtf8 path)
  void $ HTTP.httpBS req

data Service = Service
  { createContainer :: CreateContainerOptions -> IO ContainerId
  , startContainer :: ContainerId -> IO ()
  }

createService ::  Service
createService =  Service{createContainer, startContainer}
