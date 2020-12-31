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
import Data.Functor (void)


-- | An instance of an "application" server which can be used to control
-- it.
data AppServer
  = AppServer
      { serverThread :: [Async ()],
        serverPort :: Port,
        serverName :: Text,
        serverLogger :: LoggerEnv
      }

-- |Starts a new application server and returns its configuration as an `AppServer` structure.
startAppServer :: Text -> [Text] -> Port -> (LoggerEnv -> IO Application) -> IO AppServer
startAppServer serverAssignedName allowedOrigins listenPort makeApp = do
  logger <- newLog serverAssignedName
  loggerMiddleware <- runHTTPLog logger
  (realPort, thread) <- server logger loggerMiddleware
  pure $ AppServer [thread] realPort (actualServerName realPort) logger
  where

    actualServerName port =
      if serverAssignedName == "localhost" || serverAssignedName == ""
        then pack ("localhost:" <> show port)
        else serverAssignedName

    makeWarpRunner =
      if listenPort /= 0
        then pure (listenPort, Warp.run listenPort)
        else openFreePort >>= \(port, socket) -> pure (port, Warp.runSettingsSocket defaultSettings socket)

    server logger loggerMiddleware = do
      (realPort, appRunner) <- makeWarpRunner
      app <- makeApp logger
      thread <-
        async $ appRunner
          $ loggerMiddleware
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
waitServer (AppServer threads _ _ _) = void $ waitAnyCancel threads

stopServer :: AppServer -> IO ()
stopServer (AppServer threads _ _ logger) = stopLogger logger >> mapM_ cancel threads
