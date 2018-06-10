{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

main :: IO ()
main = do
  cfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock cfg app)

app :: SpockM () () () ()
app = get root hello

hello :: SpockAction db sess st ()
hello = do
  html "<h1>Hi</h1>"
