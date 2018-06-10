{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid

main :: IO ()
main = do
  cfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock cfg app)

app :: SpockM () () () ()
app = get root helloAction

helloAction :: SpockAction db sess st ()
helloAction = do
  helloInHtml

helloInHtml :: SpockActionCtx ctx db sess st ()
helloInHtml = lucid $ do
  html_ $ do
    head_ (title_ "Hello")
    body_ $ do
      h1_ "Hello!"
      p_ "Hello! Lucid"
      p_ $ do
        "I love "
        a_ [href_ "http://haskell.org"] "doggy"
        " !!!"







