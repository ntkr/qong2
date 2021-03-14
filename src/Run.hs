{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import RIO
import WebSockets 
import Game
import Util
import qualified RIO.Text as T
import Data.Aeson as Aeson
import qualified Network.WebSockets as WS

run :: RIO App ()
run = do
  env <- ask

  let host = optionsHost $ appOptions env
  let port = optionsPort $ appOptions env
  let hostport = fromString host <> ":" <> fromString (show port)

  logInfo $ "Running server on " <> hostport

  mServerState <- newMVar initialServerState
  runServer host port $ application mServerState


application 
  :: MVar State 
  -> WS.PendingConnection
  -> RIO App ()
application state pending = do

  conn <- acceptRequest pending
  logInfo "Connection received, dispatching token."

  withPingThread conn 30 (return ()) 
    $ forever $ do

      msg <- receiveData conn

      case Aeson.decode msg :: Maybe Message of

          Just msg -> updateState state conn msg >>= broadcastState

          Nothing -> sendTextData conn ("Unknown message" :: Text)
    -- WS.sendTextData conn (T.append "Testing" msg :: Text)
    -- add a disconnection function in here
    -- that removes the client after
    -- the session has closed

updateState 
  :: MVar State -> WS.Connection -> Message -> RIO App State
updateState state conn msg =
  modifyMVar state $ \s -> return . pair $ processMessage s conn msg


broadcastState :: State -> RIO App ()
broadcastState state = 
  let 
    jsonGameState = Aeson.encode . stateGameState $ state
  in 
    forM_ (statePlayers state) $ \p -> do
      sendTextData (playerConn p) jsonGameState
  
