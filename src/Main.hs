{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (json)
import GHC.Generics
import Database.Persist hiding (get)
import Database.Persist.Postgresql hiding (get)
import Database.Persist.TH
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)

type MyM = SpockM MyDb MySess () ()
type MyDb = SqlBackend
data MySess = EmptySession
type MyAction a = SpockAction MyDb MySess () a

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
  name Text
  signature Text
  UniquePerson name
  deriving Eq Read Show Generic
|]

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connString 5
  cfg <- defaultSpockCfg EmptySession (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8080 (spock cfg myapp)

myapp :: MyM
myapp = do

  get "people" $ do
    allPeople <- runSQL  $ selectList [] [Asc PersonId]
    -- json allPeople
    lucid $ do
      table_ $ do
        tr_ $ do
          th_ "name"
          th_ "signature"
        forM_ allPeople $ \person -> tr_ $ do
          td_ $ toHtml (personName $ entityVal person)
          td_ $ toHtml (personSignature $ entityVal person)

  get ("people" <//> var) $ \name -> do
    maybePerson <- runSQL $ getBy $ UniquePerson name
    case maybePerson of
      Nothing -> errorJson 2 "Can't find matching person"
      Just (Entity personId thePerson) ->
        -- json thePerson
        do
          lucid $ do
            h1_ "yey"
            h1_ $ toHtml name
            h1_ $ toHtml (personSignature thePerson)
  
  post "people" $ do
    maybePerson <- jsonBody' :: MyAction (Maybe Person)
    case maybePerson of
      Nothing -> errorJson 1 "Oops can't parse request as Person"
      Just thePerson -> do
        newId <- runSQL $ insert thePerson
        json $ object [ "result" .= String "success", "id" .= newId ]


connString :: ConnectionString
connString = "host=localhost port=5432 user=testuser dbname=testdb password=password"

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> MyAction ()
errorJson code msg = json $
  object
  [ "result" .= String "failure!"
  , "error" .= object [ "code" .= code, "message" .= msg ]
  ]


