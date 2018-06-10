{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid
import qualified Database.PostgreSQL.Simple as Pg
import qualified Data.Configurator as Cf

main :: IO ()
main = do
  connInfo <- myConnInfo "pg.cfg"
  conn <- Pg.connect connInfo
  cfg <- defaultSpockCfg () (PCConn (mkConn connInfo)) ()
  runSpock 8080 (spock cfg app)

app :: SpockM Pg.Connection () state ()
app = do
  get root helloAction

helloAction :: SpockAction db sess state ()
helloAction = do
  helloInHtml

helloInHtml :: SpockActionCtx ctx db sess state ()
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

myConnInfo :: FilePath -> IO Pg.ConnectInfo
myConnInfo path = do
  pgCfg <- Cf.load $ [Cf.Required path]
  Pg.ConnectInfo <$> Cf.require pgCfg "host"
                 <*> Cf.require pgCfg "port"
                 <*> Cf.require pgCfg "user"
                 <*> Cf.require pgCfg "password"
                 <*> Cf.require pgCfg "db"

mkConn :: Pg.ConnectInfo -> ConnBuilder Pg.Connection
mkConn connInfo =
  ConnBuilder { cb_createConn = Pg.connect connInfo
              , cb_destroyConn = Pg.close
              , cb_poolConfiguration =
                PoolCfg { pc_stripes = 1
                        , pc_resPerStripe = 5
                        , pc_keepOpenTime = 60 }
              }

