{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid
import qualified Database.PostgreSQL.Simple as Pg
import qualified Data.Configurator as Cf
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Int (Int64)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

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
  get root $ do
    (MyState ref) <- getState
    visitNum <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
    lucid $ do
      pageTemplate "my title"
      p_ $ do
        "one more line"
        p_ . toHtml $ "You are visit number " `T.append` (T.pack . show $ visitNum) `T.append` "!"
        p_ "Please put your name in the guestbook"
        p_ $ a_ [href_ "guestbook"] "GuestBook"
  get "guestbook" $ do
    guests <- runQuery (\x -> Pg.query_ x "select * from guest;" :: IO [Pg.Only T.Text])
    lucid $ do
      pageTemplate "my guestbook"
      h1_ "Guest List"
      guestTemplate guests
      h1_ "Guest Sign In"

pageTemplate :: T.Text -> Html ()
pageTemplate title = do
  html_ $ do
    head_ (title_ $ toHtml title)
    body_ $ do
      h1_ "Hello!"
      p_ "Hello! Lucid"
      p_ $ do
        "I love "
        a_ [href_ "http://haskell.org"] "doggy"
        " !!!"

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

