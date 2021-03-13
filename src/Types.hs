{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where

import RIO
import RIO.Process
import qualified Network.WebSockets as WS
import Data.Aeson as A


-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsHost :: !String
  , optionsPort :: !Int
  }


data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Types.Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })


data State = State
  { statePlayers :: ![Player]
  , stateGameState :: !GameState
  }

-- is it possible to create a type here
-- whose values can only be set once?
type GameState = [[Int]]

data Player = Player
  { playerName :: !String
  , playerConn :: !WS.Connection
  }


data IncomingMessage 
  = JoinGame { joinGameName::String }
  | LeaveGame
  | StartGame
  | Move { moveX::Int, moveY::Int }-- coordinates
  deriving (Generic, Show)

instance FromJSON IncomingMessage where
  parseJSON = withObject "message" $ \o -> do
    kind <- o .: "type"
    case kind of
      "JoinGame" -> JoinGame <$> o .: "name"
      "LeaveGame" -> return LeaveGame
      "StartGame" -> return StartGame
      "Move" -> Move <$> o .: "x" <*> o .: "y"
      _ -> fail ("unknown message type: " ++ kind)

instance ToJSON IncomingMessage where
  toJSON (JoinGame name) = object [ 
    "type" .= ("JoinGame" :: Text),
    "name" .= name ]
  toJSON LeaveGame = object [ "type" .= ("LeaveGame"  :: Text) ]
  toJSON StartGame = object [ "type" .= ("StartGame" :: Text) ]
  toJSON (Move x y) = object [ 
    "type" .= ("Move" :: Text),
    "x" .= x,
    "y" .= y ]