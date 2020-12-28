{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Werror #-}

-- | A simple logger that reads `LogEntry` from a `TBChan` and dumps JSON-formatted
-- strings to `stdout`.
module Preface.Log
  ( -- * Types
    LoggerEnv,
    module System.Log.FastLogger,

    -- * Constructor & Destructor
    newLog,
    stopLogger,
    fakeLogger,
    loggerId,

    -- * Logging functions
    logInfo,
    logError,
    withLog,
  )
where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.Chan.Unagi
  ( InChan,
    OutChan,
    newChan,
    readChan,
    writeChan,
  )
import Control.Exception.Safe (IOException, catch)
import Control.Monad (forever)
import Control.Monad.Trans (MonadIO (..))
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Time.Clock
  ( UTCTime,
    diffUTCTime,
    getCurrentTime,
  )
import System.IO (Handle, stdout)
import System.Log.FastLogger

-- | Environment to control a logger thread.
data LoggerEnv = LoggerEnv
  { logger :: Maybe Logger,
    loggerId :: Text,
    logInfo :: forall a m. (MonadIO m, ToJSON a) => a -> m (),
    logError :: forall a m. (MonadIO m, ToJSON a) => a -> m (),
    withLog :: forall a b m. (MonadIO m, ToJSON a) => a -> m b -> m b,
    stopLogger :: forall m . MonadIO m => m ()
  }

type Logger = InChan BS.ByteString

fakeLogger :: LoggerEnv
fakeLogger = LoggerEnv Nothing "foo" (const $ pure ()) (const $ pure ()) (\_ -> id) (pure ())

-- | Starts an asynchronous log-processing thread and returns an initialised `LoggerEnv`.
newLog :: (MonadIO m) => Text -> m LoggerEnv
newLog loggerId = liftIO $ do
  (inchan, outchan) <- newChan
  loggerThread <- async $ runLog outchan stdout
  let logger = Just inchan
      logInfo a = logEvent' inchan loggerId a
      logError a = logError' inchan loggerId a
      withLog a act = withLog' inchan loggerId a act
  return $ LoggerEnv {stopLogger =  liftIO (cancel loggerThread),.. }

runLog :: OutChan BS.ByteString -> Handle -> IO a
runLog chan hdl =
  forever $ do
    toLog <- readChan chan
    -- ignore IOException in order to not kill the logging thread even if the output
    -- `Handle` is closed
    (BS.hPutStr hdl . (<> "\n") $ toLog) `catch` \(_ :: IOException) -> pure ()

logEvent' :: (ToJSON a, MonadIO m) => Logger -> Text -> a -> m ()
logEvent' chan logId message = liftIO $ do
  ts <- getCurrentTime
  tid <- myThreadId
  writeChan chan $
    LBS.toStrict $
      encode $
        object
          [ "timestamp" .= ts,
            "loggerId" .= logId,
            "threadId" .= show tid,
            "message" .= message
          ]

withLog' :: (ToJSON a, MonadIO m) => Logger -> Text -> a -> m b -> m b
withLog' chan logId message act = do
  startTime <- liftIO getCurrentTime
  logStart chan logId startTime message
  b <- act
  endTime <- liftIO getCurrentTime
  logEnd chan logId startTime endTime message
  pure b

logStart :: (ToJSON a, MonadIO m) => Logger -> Text -> UTCTime -> a -> m ()
logStart chan logId ts command = liftIO $ do
  tid <- myThreadId
  writeChan chan $
    LBS.toStrict $
      encode $
        object
          [ "timestamp" .= ts,
            "loggerId" .= logId,
            "threadId" .= show tid,
            "message" .= command
          ]

logEnd :: (ToJSON a, MonadIO m) => Logger -> Text -> UTCTime -> UTCTime -> a -> m ()
logEnd chan logId ts en outcome = liftIO $ do
  tid <- myThreadId
  writeChan chan $
    LBS.toStrict $
      encode $
        object
          [ "timestamp" .= en,
            "loggerId" .= logId,
            "threadId" .= show tid,
            "durationMs" .= ((diffUTCTime en ts) * 1000),
            "message" .= outcome
          ]

logError' :: (ToJSON a, MonadIO m) => Logger -> Text -> a -> m ()
logError' chan logId err = liftIO $ do
  ts <- getCurrentTime
  tid <- myThreadId
  writeChan chan $
    LBS.toStrict $
      encode $
        object
          [ "timestamp" .= ts,
            "loggerId" .= logId,
            "threadId" .= show tid,
            "error" .= err
          ]
