{-# LANGUAGE NoImplicitPrelude #-}
module WebSockets 
    ( runServer 
    ) where
--     , withPingThread
--     , receiveData
--     , sendTextData
--     ) where

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