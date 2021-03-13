{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import Options.Applicative
import qualified Paths_qong2_server

optionsParser :: Parser Options
optionsParser = Options
             <$> switch 
                 ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
                 )
             <*> strOption
                 ( long "host"
                <> short 'h'
                <> metavar "HOST"
                <> help "Run the server on this host"
                <> value "localhost"
                 )
             <*> option auto 
                 ( long "port" 
                <> short 'p'
                <> metavar "PORT"
                <> help "Run the server on this port"
                <> value 8123
                 )

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_qong2_server.version)
    ""
    "Qong2 is a tiny game server library"
    optionsParser
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run

