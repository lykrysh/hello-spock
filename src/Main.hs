{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid
import qualified Database.PostgreSQL.Simple as Pg
import qualified Data.Configurator as Cf
import Data.IORef (IORef, newIORef)
import Data.Int (Int64)

type MyM = SpockM MyDb MySess MyState ()
type MyDb = Pg.Connection
data MySess = EmptySession
data MyState =  MyState (IORef Int)

main :: IO ()
main = do
  connInfo <- myConnInfo "pg.cfg"
  conn <- Pg.connect connInfo
  createTable conn

  ref <- newIORef 0
  cfg <- defaultSpockCfg EmptySession (PCConn (mkConn connInfo)) (MyState ref)
  runSpock 8080 (spock cfg app)

app :: MyM
app = do
  get root helloAction

helloAction :: SpockAction MyDb MySess MyState ()
helloAction = do
  helloInHtml

helloInHtml :: SpockActionCtx ctx MyDb MySess MyState ()
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

createTable :: MyDb -> IO Int64
createTable c =
  Pg.execute_ c "create table if not exists guest (name varchar(80));"

myConnInfo :: FilePath -> IO Pg.ConnectInfo
myConnInfo path = do
  pgCfg <- Cf.load $ [Cf.Required path]
  Pg.ConnectInfo <$> Cf.require pgCfg "host"
                 <*> Cf.require pgCfg "port"
                 <*> Cf.require pgCfg "user"
                 <*> Cf.require pgCfg "password"
                 <*> Cf.require pgCfg "db"

mkConn :: Pg.ConnectInfo -> ConnBuilder MyDb
mkConn connInfo =
  ConnBuilder { cb_createConn = Pg.connect connInfo
              , cb_destroyConn = Pg.close
              , cb_poolConfiguration =
                PoolCfg { pc_stripes = 1
                        , pc_resPerStripe = 5
                        , pc_keepOpenTime = 60 }
              }

