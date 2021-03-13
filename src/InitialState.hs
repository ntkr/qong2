{-# LANGUAGE NoImplicitPrelude #-}
module InitialState where

import Types
import RIO

initialServerState :: State
initialServerState = State 
    { statePlayers = []
    , stateGameState = [[]] }