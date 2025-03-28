module Docker where

import qualified Codec.Serialise as Serialise
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock.POSIX as Time
import qualified Network.HTTP.Simple as HTTP
import RIO
import RIO.Text (unpack)
import RIO.Text.Partial (splitOn)
import qualified Socket

data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerExitCode
  | ContainerOther Text
  deriving (Eq, Show)

instance Aeson.FromJSON ContainerStatus where
  parseJSON = Aeson.withObject "ContainerStatus" \obj -> do
    state <- obj .: "State"
    status <- state .: "Status"
    case (status :: String) of
      "running" -> return ContainerRunning
      "exited" -> ContainerExited . ContainerExitCode <$> state .: "ExitCode"
      _ -> return (ContainerOther "unknown status")

data Image = Image {name :: Text, tag :: Text}
  deriving (Eq, Generic, Serialise.Serialise)

instance Aeson.FromJSON Image where
  parseJSON = Aeson.withText "parse-image" \text -> case splitOn ":" text of
    (image : tag : _) -> return $ Image image tag
    _ -> error "invalid image format"

instance Show Image where
  show image = unpack $ image.name <> ":" <> image.tag

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show, Generic)
  deriving newtype (Serialise.Serialise)

newtype ContainerId = ContainerId {id :: Text}
  deriving newtype (Eq, Show, Aeson.FromJSON, Serialise.Serialise)

newtype Script = Script {src :: Text}

data CreateContainerOptions = CreateContainerOptions
  { image :: Image
  , script :: Script
  , volume :: Volume
  }

type RequestBuilder = Text -> HTTP.Request

data CreateContainerResponse = CreateContainerResponse
  { id :: ContainerId
  , warnings :: [String]
  }
  deriving (Show)

instance Aeson.FromJSON CreateContainerResponse where
  parseJSON =
    Aeson.withObject
      "CreateContainerResponse"
      \obj -> CreateContainerResponse <$> (obj .: "Id") <*> (obj .: "Warnings")

newtype Volume = Volume {name :: Text}
  deriving (Eq, Show, Generic)
  deriving newtype (Serialise.Serialise)

instance Aeson.FromJSON Volume where
  parseJSON = Aeson.withObject "Volume" $ fmap Volume <$> (.: "Name")

data FetchLogsOptions = FetchLogsOptions
  { container :: ContainerId
  , since :: Time.POSIXTime
  , until :: Time.POSIXTime
  }

parseResponse :: (Aeson.FromJSON a) => HTTP.Response ByteString -> a
parseResponse res = either error id $ Aeson.eitherDecodeStrict $ HTTP.getResponseBody res

createContainer :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer request options = do
  let bind = options.volume.name <> ":/app"
  let body =
        Aeson.object
          [ ("Image", Aeson.toJSON $ show options.image)
          , ("Tty", Aeson.toJSON True)
          , ("Labels", Aeson.object [("quad", "")])
          , ("Cmd", "echo \"$QUAD_SCRIPT\" | /bin/sh")
          , ("Env", Aeson.toJSON ["QUAD_SCRIPT=" <> options.script.src])
          , ("Entrypoint", Aeson.toJSON ["/bin/sh" :: String, "-c"])
          , ("WorkingDir", "/app")
          , ("HostConfig", Aeson.object [("Binds", Aeson.toJSON [bind])])
          ]
  let req =
        request "/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
  res <- HTTP.httpBS req
  let CreateContainerResponse{id = _id} = parseResponse res
  return _id

startContainer :: RequestBuilder -> ContainerId -> IO ContainerId
startContainer request (ContainerId _id) = HTTP.httpBS req >> return (ContainerId _id)
 where
  req = request path & HTTP.setRequestMethod "POST"
  path = "/containers/" <> _id <> "/start"

containerStatus :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatus request (ContainerId _id) = parseResponse <$> HTTP.httpBS req
 where
  path = "/containers/" <> _id <> "/json"
  req = request path & HTTP.setRequestMethod "GET"

createVolume :: RequestBuilder -> IO Volume
createVolume request = parseResponse <$> HTTP.httpBS req
 where
  req =
    request "/volumes/create"
      & HTTP.setRequestMethod "POST"
      & HTTP.setRequestBodyJSON body
  body = Aeson.object [("Labels", Aeson.object [("quad", "")])]

fetchLogs :: RequestBuilder -> FetchLogsOptions -> IO ByteString
fetchLogs request options = do
  let timestampToText t = tshow (round t :: Int)
  let url =
        "/containers/"
          <> options.container.id
          <> "/logs?stdout=true&stderr=true&since="
          <> timestampToText options.since
          <> "&until="
          <> timestampToText options.until
  res <- HTTP.httpBS $ request url
  return $ HTTP.getResponseBody res

pullImage :: RequestBuilder -> Image -> IO ()
pullImage request image = void $ HTTP.httpBS req
 where
  req = request url & HTTP.setRequestMethod "POST"
  url = "/images/create?tag=" <> image.tag <> "&fromImage=" <> image.name

data Service = Service
  { createContainer :: CreateContainerOptions -> IO ContainerId
  , startContainer :: ContainerId -> IO ContainerId
  , containerStatus :: ContainerId -> IO ContainerStatus
  , createVolume :: IO Volume
  , fetchLogs :: FetchLogsOptions -> IO ByteString
  , pullImage :: Image -> IO ()
  }

createService :: IO Service
createService = do
  manager <- Socket.newManager "/var/run/docker.sock"
  let request :: RequestBuilder
      request path =
        HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath (encodeUtf8 $ "/v1.47" <> path)
  return
    Service
      { createContainer = createContainer request
      , startContainer = startContainer request
      , containerStatus = containerStatus request
      , createVolume = createVolume request
      , fetchLogs = fetchLogs request
      , pullImage = pullImage request
      }
