module Server where

import qualified Codec.Serialise as Serialise
import Core
import qualified JobHandler
import RIO
import qualified Web.Scotty as Scotty

data Config = Config {port :: Int}

run :: Config -> JobHandler.Service -> IO ()
run config handler = Scotty.scotty config.port do
  Scotty.post "/agent/pull" do
    cmd <- liftIO handler.dispatchCmd
    Scotty.raw $ Serialise.serialise cmd
  Scotty.post "/agent/send" do
    msg <- Serialise.deserialise <$> Scotty.body
    liftIO $ handler.processMsg msg
    Scotty.json ("message processed" :: Text)
  pure ()
