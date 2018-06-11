{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToRow as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg
import qualified Data.Configurator as Cf
import Data.Text (Text, pack)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Int (Int64)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)
import BasePrelude

type Server a = SpockM () () ServerState ()
newtype ServerState = ServerState { movies :: IORef [Movie]}
data Movie = Movie { title :: Text, signature :: Text, _id :: Integer }

main :: IO ()
main = do
  connInfo <- myConnInfo "pg.cfg"
  conn <- Pg.connect connInfo

  st <- ServerState <$>
    newIORef [ Movie "movie one" "140592938796" 1
             , Movie "movie two" "p02924787568" 2
             ]

  cfg <- defaultSpockCfg () (PCConn (mkConn connInfo)) st
  runSpock 8080 (spock cfg myapp)

myapp :: SpockM Pg.Connection () ServerState ()
myapp = do
  get root $ do
    movies' <- runQuery (\conn -> allMovies conn)
    lucid $ do
      renderHtml movies'
      putMoviesForm
  post root $ do
    title <- param' "title"
    signature <- param' "signature"
    _id <- liftIO $ randomRIO (1,10)
    _ <- runQuery $ insertMovie $ Movie title signature _id
    moviesRef <- movies <$> getState
    liftIO $ atomicModifyIORef' moviesRef $ \movies ->
      (movies <> [Movie title signature _id], ())
    redirect "/"

allMovies :: Pg.Connection -> IO [Movie]
allMovies c = Pg.query_ c "select title, signature, _id from movies"

instance Pg.FromRow Movie where
  fromRow = Movie <$> Pg.field <*> Pg.field <*> Pg.field

renderHtml :: [Movie] -> Html ()
renderHtml notes' = do
  html_ $ do
    head_ (title_ "Movies")
    body_ $ do
      h1_ "Movies!"
      p_ $ do
        "I love "
        a_ [href_ "http://haskell.org"] "doggy"
        " !!!"
      ul_ $ forM_ notes' $ \movie -> li_ $ do
        toHtml (title movie)
        ": "
        toHtml (signature movie)

putMoviesForm :: Html ()
putMoviesForm = do
  h1_ "Add new movie"
  form_ [method_ "post"] $ do
    label_ $ do
      "Title: "
      input_ [name_ "title"]
    label_ $ do
      "Signature: "
      textarea_ [name_ "signature"] ""
    input_ [type_ "submit", value_ "Add movie"]

insertMovie :: Movie -> Pg.Connection -> IO Int64
insertMovie n conn =
  Pg.executeMany
    conn
    "insert into movies (title, signature, _id) values (?, ?, ?)"
    [n]

instance Pg.ToRow Movie where
  toRow n = Pg.toField <$> [title n, signature n, pack $ show $_id n]

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

