module Agent where

import qualified Codec.Serialise as Serialise
import Core
import qualified Network.HTTP.Simple as HTTP
import RIO hiding (log)
import qualified Runner
import qualified System.Log.Logger as Logger

data Cmd = StartBuild BuildNumber Pipeline
  deriving (Eq, Show, Generic, Serialise.Serialise)

data Msg
  = LogCollected BuildNumber Log
  | BuildUpdated BuildNumber Build
  deriving (Eq, Show, Generic, Serialise.Serialise)

newtype Config = Config
  { endpoint :: String
  }

run :: Config -> Runner.Service -> IO ()
run config runner = forever do
  endpoint <- HTTP.parseRequest config.endpoint
  let req =
        endpoint
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestPath "/agent/pull"
  do
    res <- HTTP.httpLBS req
    let cmd = Serialise.deserialise (HTTP.getResponseBody res) :: Maybe Cmd
    traverse_ (runCommand config runner) cmd
    `catch` \e -> do
      Logger.warningM "quad.agent" "Server offline, waiting..."
      Logger.warningM "quad.agent" $ show (e :: HTTP.HttpException)
  threadDelay (1 * 1000 * 1000)

runCommand :: Config -> Runner.Service -> Cmd -> IO ()
runCommand config runner = \case
  StartBuild number pipeline -> do
    let hooks =
          Runner.Hooks
            { logCollected = sendMessage config . LogCollected number
            , buildUpdated = sendMessage config . BuildUpdated number
            }
    build <- runner.prepareBuild pipeline
    void $ runner.runBuild hooks build

sendMessage :: Config -> Msg -> IO ()
sendMessage config msg = do
  base <- HTTP.parseRequest config.endpoint
  let body = Serialise.serialise msg
  let req =
        base
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestPath "/agent/send"
          & HTTP.setRequestBodyLBS body
  void $ HTTP.httpBS req
