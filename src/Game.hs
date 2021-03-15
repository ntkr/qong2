{-# LANGUAGE NoImplicitPrelude #-}
module Game where

import Types
import RIO
import qualified Network.WebSockets as WS
import Util

    
processMessage 
  :: State -> WS.Connection -> Message -> State
processMessage state conn msg =
  case msg of

    Move x y ->
      state {
        stateGameState = moveAt x y (stateGameState state)
      }
      where
        -- woof, this is clunky
        moveAt movex movey s = 
            flip mapWithIndex s
                (\statex col -> 
                    if statex /= movex 
                    then col 
                    else flip mapWithIndex col
                        (\statey mark ->
                            if statey /= movey
                            then mark
                            else X))
