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

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

newtype CreateContainerOptions = CreateContainerOptions {image :: Image}

data ResponseBody = ResponseBody
  { id :: String
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
parseResponse res = do
  let result = do Aeson.eitherDecodeStrict $ HTTP.getResponseBody res
  case result of
    Left e -> error e
    Right parsed -> parsed

createContainer :: CreateContainerOptions -> IO String
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
