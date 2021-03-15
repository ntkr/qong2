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
  } deriving (Generic, ToJSON)

-- is it possible to create a type here
-- whose values can only be set once?
data Mark = None | X | O deriving (Generic, ToJSON, FromJSON)
type GameState = [[Mark]]

data Player = Player
  { playerNumber :: !Int
  , playerConn :: !WS.Connection
  }

instance ToJSON Player where
  toJSON (Player playerNumber _) = object [
    "number" .= playerNumber ]


data Message 
  = StartGame
  | Move { moveX::Int, moveY::Int }-- coordinates
  deriving (Generic, Show)

instance FromJSON Message where
  parseJSON = withObject "message" $ \o -> do
    kind <- o .: "type"
    case kind of
      "StartGame" -> return StartGame
      "Move" -> Move <$> o .: "x" <*> o .: "y"
      _ -> fail ("unknown message type: " ++ kind)

instance ToJSON Message where
  toJSON StartGame = object [ "type" .= ("StartGame" :: Text) ]
  toJSON (Move x y) = object [ 
    "type" .= ("Move" :: Text),
    "x" .= x,
    "y" .= y ]