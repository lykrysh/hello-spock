{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Text
import Data.Semigroup ((<>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (json)
import GHC.Generics
import Database.Persist hiding (get)
import Database.Persist.Postgresql hiding (get)
import Database.Persist.TH
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)

type MyDb = SqlBackend
data MySess = EmptySession

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
  name Text
  signature Text
  UniquePerson name
  deriving Eq Read Show Generic
|]

data MyState = MyState { persons :: IORef [Person] }
type MyM = SpockM MyDb MySess MyState ()
type MyAction a = SpockAction MyDb MySess MyState a

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connString 5
  state <- MyState <$> newIORef []
  cfg <- defaultSpockCfg EmptySession (PCPool pool) state 
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8080 (spock cfg myapp)

myapp :: MyM
myapp = do
  get "people" $ do
    allPeople <- runSQL  $ selectList [] [Asc PersonId]
    -- json allPeople
    lucid $ do
      pageTemplate "people"
      renderList allPeople
      h1_ "Add more person"
      renderAddForm "addperson"
  get ("people" <//> var) $ \name -> do
    maybePerson <- runSQL $ getBy $ UniquePerson name
    case maybePerson of
      Nothing -> errorJson 2 "Can't find matching person"
      Just (Entity personId thePerson) ->
        -- json thePerson
        do
          lucid $ do
            pageTemplate name
            h1_ "yey"
            h1_ $ toHtml (personName thePerson)
            h1_ $ toHtml (personSignature thePerson)
  post "addperson" $ do
    name <- param' "name"
    signature <- param' "signature"
    maybePerson <- json $ object [ "name" .= String name, "signature" .= String signature ] :: MyAction (Maybe Person)
    case maybePerson of
      Nothing -> errorJson 1 "Oops can't parse request as Person"
      Just thePerson -> do
        _ <- runSQL $ insert thePerson
        personRef <- persons <$> getState
        liftIO $ atomicModifyIORef' personRef $ \g -> (g <> [Person name signature], ())
    redirect "people"




connString :: ConnectionString
connString = "host=localhost port=5432 user=testuser dbname=testdb password=password"

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

pageTemplate :: Text -> Html ()
pageTemplate title = do
  doctype_
  html_ $ do
    head_ (title_ $ toHtml title)
    body_ $ do
      h1_ "Hello!"

renderList :: [Entity Person] -> Html ()
renderList xs = do
  table_ $ do
    tr_ $ do
      th_ "name"
      th_ "signature"
    forM_ xs $ \person -> tr_ $ do
      td_ $ toHtml (personName $ entityVal person)
      td_ $ toHtml (personSignature $ entityVal person)

renderAddForm :: Text -> Html ()
renderAddForm action = do
  form_ [action_ action, method_ "post"] $ do
    label_ $ do
      "Name: "
      input_ [type_ "text", name_ "name"]
    label_ $ do
      "Signature: "
      textarea_ [name_ "signature"] ""
    input_ [type_ "submit", value_ "Add Person"]

errorJson :: Int -> Text -> MyAction ()
errorJson code msg = json $
  object
  [ "result" .= String "failure!"
  , "error" .= object [ "code" .= code, "message" .= msg ]
  ]


