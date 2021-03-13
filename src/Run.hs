{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import RIO
import WebSockets 
import qualified RIO.Text as T
import Data.Aeson as Aeson
import qualified Network.WebSockets as WS

run :: RIO App ()
run = do
  env <- ask

  let host = optionsHost $ appOptions env
  let port = optionsPort $ appOptions env

  mServerState <- newMVar initialServerState
  logInfo "Running server"

  -- liftIO $ WS.runServer host port (application mServerState)
  -- this needs to be a WS.PendingConnection -> IO () 
  -- not sure how to use RIO monad here
  --                              v
  runServer host port $ application mServerState
  -- withRunInIO $ \runInIO ->
  --   WS.runServer (runInIO . (application mServerState))


application 
  :: MVar State 
  -> WS.PendingConnection
  -> RIO App ()
application state pending = do
  -- env <- ask
  conn <- liftIO $ WS.acceptRequest pending
  logInfo "Connection Received"
  liftIO $ WS.withPingThread conn 30 (return ()) $ forever $ do
    msg <- WS.receiveData conn
    case Aeson.decode msg :: Maybe IncomingMessage of

        -- Just msg -> updateState state conn msg >>= broadcastState
        Just msg -> 
          updateState state conn msg >>= liftIO . broadcastState
          -- WS.sendTextData conn ((playerName . head . statePlayers) :: Text)


        Nothing -> liftIO $ WS.sendTextData conn ("Invalid message" :: Text)
    -- WS.sendTextData conn (T.append "Testing" msg :: Text)
    -- add a disconnection function in here
    -- that removes the client after
    -- the session has closed

updateState 
  :: MVar State 
  -> WS.Connection 
  -> IncomingMessage 
  -> IO State
updateState state conn msg =
  modifyMVar state $ \s -> return (processMessage s conn msg, s)

processMessage 
  :: State
  -> WS.Connection
  -> IncomingMessage
  -> State
processMessage state conn (JoinGame name) =
  state { 
    statePlayers = Player name conn : statePlayers state
  }
processMessage state conn (Move x y) = state


broadcastState :: State -> IO ()
broadcastState state = 
  let 
    jsonGameState = Aeson.encode . stateGameState $ state
  in 
    forM_ (statePlayers state) $ \p -> do
      WS.sendTextData (playerConn p) jsonGameState
  
