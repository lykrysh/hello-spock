{-# LANGUAGE OverloadedStrings #-}
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
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Int (Int64)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

type MyM = SpockM MyDb MySess MyState ()
type MyDb = Pg.Connection
data MySess = EmptySession
data MyState =  MyState { guests :: IORef [Guest] }
data Guest = Guest { name :: T.Text, signature :: T.Text }

main :: IO ()
main = do
  connInfo <- myConnInfo "pg.cfg"
  conn <- Pg.connect connInfo
  createTable conn

  state <- MyState <$> newIORef []
  cfg <- defaultSpockCfg EmptySession (PCConn (mkConn connInfo)) state
  runSpock 8080 (spock cfg myapp)

myapp :: MyM
myapp = do
  get root $ do
    lucid $ do
      pageTemplate "my title"
      p_ $ do
        "one more line"
        p_ "Please put your name in the guestbook"
        p_ $ a_ [href_ "addguest"] "Add Guest Here"
  get "addguest" $ do
    guests <- runQuery (\conn -> retrieveAllGuests conn)
    lucid $ do
      pageTemplate "my guestbook"
      h1_ "Guest List"
      renderTable guests
      h1_ "Add Guest"
      form_ [action_ "goguest", method_ "post"] $ do
        label_ $ do
          "Name: "
          input_ [type_ "text", name_ "name"]
        label_ $ do
          "Signature: "
          textarea_ [name_ "signature"] ""
        input_ [type_ "submit", value_ "Add Guest"]
  post "goguest" $ do
    name <- param' "name"
    signature <- param' "signature"
    _ <- runQuery $ insertGuest $ Guest name signature
    guestRef <- guests <$> getState 
    liftIO $ atomicModifyIORef' guestRef $ \g -> (g <> [Guest name signature], ())
    redirect "addguest"

retrieveAllGuests :: MyDb -> IO [Guest]
retrieveAllGuests c = Pg.query_ c "select name, signature from guest"

instance Pg.FromRow Guest where
  fromRow = Guest <$> Pg.field <*> Pg.field

pageTemplate :: T.Text -> Html ()
pageTemplate title = do
  doctype_
  html_ $ do
    head_ (title_ $ toHtml title)
    body_ $ do
      h1_ "Hello!"
      p_ "Hello! Lucid"
      p_ $ do
        "I love "
        a_ [href_ "http://haskell.org"] "doggy"
        " !!!"

renderTable :: [Guest] -> Html ()
renderTable xs = do
  table_ $ do
    tr_ $ do
      th_ "Name"
      th_ "Signature"
    forM_ xs $ \guest -> tr_ $ do
      td_ $ toHtml (name guest)
      td_ $ toHtml (signature guest)

insertGuest :: Guest -> MyDb -> IO Int64
insertGuest g conn =
  Pg.executeMany conn "insert into guest (name, signature) values (?, ?)" [g]

instance Pg.ToRow Guest where
  toRow n = Pg.toField <$> [name n, signature n]

guestTemplate :: [Pg.Only T.Text] -> Html ()
guestTemplate xs = do
  table_ $ do
    tr_ $ do
      th_ "Guest"
    sequence_ $ map f xs
      where f :: Monad m => Pg.Only T.Text -> HtmlT m ()
            f (Pg.Only x) = do
              tr_ $ do
                td_ $ toHtml x

createTable :: MyDb -> IO Int64
createTable c =
  Pg.execute_ c "create table if not exists guest (name varchar(80), signature varchar(80));"

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

