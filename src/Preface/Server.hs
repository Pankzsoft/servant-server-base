module Preface.Server where

import Control.Concurrent.Async
import Data.Aeson (Value, decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Default
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Network.CORS
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Preface.Log
import Network.Wai(Application)

data AppServer
  = AppServer
      { serverThread :: Maybe (Async ()),
        serverPort :: Port,
        serverName :: Text
      }

startAppServer :: Text -> [Text] -> Port -> IO Application -> IO AppServer
startAppServer serverAssignedName allowedOrigins listenPort makeApp = do
  logger <- newLog serverAssignedName
  loggerMiddleware <- runHTTPLog logger
  (realPort, thread) <- server loggerMiddleware
  pure $ AppServer (Just thread) realPort (actualServerName realPort)
  where

    actualServerName port =
      if serverAssignedName == "localhost" || serverAssignedName == ""
        then pack ("localhost:" <> show port)
        else serverAssignedName

    makeWarpRunner =
      if listenPort /= 0
        then pure (listenPort, Warp.run listenPort)
        else openFreePort >>= \(port, socket) -> pure (port, Warp.runSettingsSocket defaultSettings socket)

    server logger = do
      (realPort, appRunner) <- makeWarpRunner
      app <- makeApp
      thread <-
        async $ appRunner
          $ logger
          $ rejectInvalidHost (encodeUtf8 $ actualServerName realPort)
          $ handleCors (fmap encodeUtf8 allowedOrigins)
          $ app
      pure (realPort, thread)

    runHTTPLog logger =
      mkRequestLogger $
        def
          { outputFormat = CustomOutputFormatWithDetails formatAsJSON,
            destination = Callback (\str -> logInfo logger (decode @Value $ fromStrict $ fromLogStr str))
          }

-- | Wait for the server to terminate its execution.
--
--  * If the server's inner thread throws an execution, it will be rethrown.
--  * If the server is not started, it returns immediately.
--
-- This a simple wrapper over `Control.Concurrent.Async.wait` in order to alleviate
-- the need to have an explicitly dependency on it.
waitServer :: AppServer -> IO ()
waitServer (AppServer (Just thread) _ _) = wait thread
waitServer _ = pure ()

stopServer :: AppServer -> IO ()
stopServer (AppServer (Just thread) _ _) = cancel thread
stopServer _ = pure ()
