# qong2-server

This is an incomplete game server for a real-time multiplayer elimination-style pong game. The initial implementation was written with an Elm frontend (github.com/ntkr/qong-client) and a Haskell backend (github.com/ntkr/qong-server). Due to the complexity of implementing the look-ahead logic in both languages those approaches were abandoned for a singular approach in Haskell.

## Execute  

* Run `stack exec -- qong2-server-exe` to see "We're inside the application!"
* With `stack exec -- qong2-server-exe --verbose` you will see the same message, with more logging.

## Run tests

`stack test`
