{-# LANGUAGE NoImplicitPrelude #-}
module WebSockets 
    ( runServer 
    , acceptRequest
    , withPingThread
    , sendTextData
    , receiveData
    ) where

import RIO
import qualified Network.WebSockets as WS

runServer
  :: MonadUnliftIO m
  => String
  -> Int
  -> (WS.PendingConnection -> m ())
  -> m ()
runServer host port app =
  withRunInIO $ \runInIO -> WS.runServer host port (runInIO . app)

acceptRequest
  :: MonadUnliftIO m
  => WS.PendingConnection
  -> m WS.Connection
acceptRequest = liftIO . WS.acceptRequest

withPingThread
  :: MonadUnliftIO m
  => WS.Connection
  -> Int
  -> m ()
  -> m a
  -> m a
withPingThread conn x y z= 
  withRunInIO $ \runInIO ->
    WS.withPingThread conn x (runInIO y) (runInIO z)

sendTextData
  :: (MonadUnliftIO m, WS.WebSocketsData a)
  => WS.Connection
  -> a
  -> m ()
sendTextData conn wsdata =
  liftIO $ WS.sendTextData conn wsdata

receiveData
  :: (MonadUnliftIO m, WS.WebSocketsData a)
  => WS.Connection
  -> m a
receiveData =
  liftIO . WS.receiveData