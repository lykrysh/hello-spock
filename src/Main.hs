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

import Db
import GHC.Generics
import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import Data.Aeson hiding (json)
import Control.Monad (forM_)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist hiding (get)
import Database.Persist.Postgresql hiding (get)
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Keyword json
  word Text
  UniqueKeyword word
  deriving Show
Film json
  title Text
  signature Text
  author Text
  year Int
  length Int
  UniqueFilm title
  deriving Eq Read Show Generic
KeywordFilm json
  keyword KeywordId
  film FilmId
  deriving Show
Watch json
  film FilmId
  viewerip Text
  when UTCTime
  finished Bool
  deriving Show
|]

type MyDb = SqlBackend
data MySess = EmptySession
type MyM = SpockM MyDb MySess () ()
type MyAction a = SpockAction MyDb MySess () a

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connString 5
  cfg <- defaultSpockCfg EmptySession (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8080 (spock cfg myapp)

myapp :: MyM
myapp = do
  get (var) $ \title -> do
    maybeFilm <- runSQL $ getBy $ UniqueFilm title
    case maybeFilm of
      Nothing -> errorJson 2 "Can't find matching film"
      Just (Entity filmId theFilm) ->
        do
          lucid $ do
            pageTemplate title
            h1_ "yey"
            h1_ $ toHtml (filmTitle theFilm)
            h1_ $ toHtml (filmSignature theFilm)
  get "admin" $ do
    allKeywords <- runSQL  $ selectList [] [Asc KeywordId]
    allFilms <- runSQL  $ selectList [] [Asc FilmId]
    allKeywordsFilms <- runSQL  $ selectList [] [Asc KeywordFilmId]
    lucid $ do
      pageTemplate "admin"
      renderKeywords allKeywords
      renderFilms allFilms
      renderKeywordsFilms allKeywordsFilms
  post "admin" $ do
    maybeFilm <- jsonBody' :: MyAction (Maybe Film)
    case maybeFilm of
      Nothing -> errorJson 1 "Oops can't parse request as Film"
      Just theFilm -> do
        newId <- runSQL $ insert $ theFilm
        lucid $ do
          h1_ $ toHtml (show newId)

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
      h1_ $ toHtml title

renderKeywords :: [Entity Keyword] -> Html ()
renderKeywords xs = do
  h1_ "keyword"
  table_ $ do
    tr_ $ do
      th_ "KeywordId"
      th_ "word"
    forM_ xs $ \kw -> tr_ $ do
      td_ $ toHtml (show $ fromSqlKey $ entityKey kw)
      td_ $ toHtml (keywordWord $ entityVal kw)

renderFilms :: [Entity Film] -> Html ()
renderFilms xs = do
  h1_ "film"
  table_ $ do
    tr_ $ do
      th_ "FilmId"
      th_ "title"
      th_ "signature"
      th_ "author"
      th_ "year"
      th_ "length"
    forM_ xs $ \film -> tr_ $ do
      td_ $ toHtml (show $ fromSqlKey $ entityKey film)
      td_ $ toHtml (filmTitle $ entityVal film)
      td_ $ toHtml (filmSignature $ entityVal film)
      td_ $ toHtml (filmAuthor $ entityVal film)
      td_ $ toHtml (show $ filmYear $ entityVal film)
      td_ $ toHtml (show $ filmLength $ entityVal film)
 
renderKeywordsFilms :: [Entity KeywordFilm] -> Html ()
renderKeywordsFilms xs = do
  h1_ "keyword_film"
  table_ $ do
    tr_ $ do
      th_ "KeywordFilmId"
      th_ "keyword"
      th_ "film"
    forM_ xs $ \kwfm -> tr_ $ do
      td_ $ toHtml (show $ fromSqlKey $ entityKey kwfm)
      td_ $ toHtml (show $ fromSqlKey $ keywordFilmKeyword $ entityVal kwfm)
      td_ $ toHtml (show $ fromSqlKey $ keywordFilmFilm $ entityVal kwfm)
 
errorJson :: Int -> Text -> MyAction ()
errorJson code msg = json $
  object
  [ "result" .= String "failure!"
  , "error" .= object [ "code" .= code, "message" .= msg ]
  ]
