{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (get, delete) -- Avoid naming clashes with Spock
import qualified Database.Persist        as P
import           Database.Persist.Sqlite hiding (get, delete)
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json -- The json keyword will make Persistent generate sensible ToJSON and FromJSON instances for us.
  name Text
  age Int
  deriving Show
|]

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8080 (spock spockCfg app)

app :: SpockM SqlBackend () () ()
app = do
  get "person" $ do
    allPeople <- runSQL $ selectList [] [Asc PersonId]
    json allPeople
  post "person" $ do
    maybePerson <- jsonBody
    case (maybePerson :: Maybe Person) of
      Nothing        -> text "Failed to parse request body as Person"
      Just thePerson -> do
        runSQL $ insert thePerson
        text "Success!"
  get ("person" <//> var) $ \id' -> do
    mPerson <- runSQL $ P.get id'
    case (mPerson :: Maybe Person) of
      Nothing -> text ("Could not find a person with an id of " <> pack (show id'))
      Just thePerson -> json thePerson
  put ("person" <//> var) $ \id' -> do
    maybePerson <- jsonBody
    case (maybePerson :: Maybe Person) of
      Nothing        -> text "Failed to parse request body as Person"
      Just thePerson -> do
        runSQL $ replace id' thePerson
        text "Success!"
  delete ("person" <//> var) $ \id' -> do
    runSQL $ P.delete (id' :: PersonId)
    text "Success!"

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn
