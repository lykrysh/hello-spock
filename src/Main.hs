module Main where

import Web.Spock
import Web.Spock.Config

main :: IO ()
main = do
  cfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock cfg app)

app :: SpockM () () () ()
app = return ()
