{-
Solution 1 involves just adding the put and delete actions

Solution 2 requires adding http-types to your Spock-rest.cabal file and
importing Network.HTTP.Types.Status. See the setStatus calls in all
route actions.

Solution 3 involves writing a custom errorHandler which we have called
jsonErrorHandler and changing our spockCfg definition to use it.  You can
see that our jsonErrorHandler type signature is identical to the signature
for spc_errorHandler. To use errorJson in both our Spock actions and our
jsonErrorHandler, we need to use the MonadIO typeclass to support both
SpockAction's WebStateM context and spc_errorHandler's IO context.
This requires importing MonadIO from Control.Monad.IO.Class and changing
the errorJson type signature.
-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson                hiding (json)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
import           Data.Text.Encoding        (decodeUtf8)
import           GHC.Generics

import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Logger      (LoggingT, runStdoutLoggingT)
import           Database.Persist          hiding (delete, get)
import qualified Database.Persist          as P
import           Database.Persist.Sqlite   hiding (delete, get)
import           Database.Persist.TH
import           Network.HTTP.Types.Status

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Person json -- The json keyword will make Persistent generate sensible ToJSON and FromJSON instances for us.
  name Text
  age Int
  deriving Show
|]

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg' <- defaultSpockCfg () (PCPool pool) ()
  let spockCfg = spockCfg' {spc_errorHandler = jsonErrorHandler}
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8080 (spock spockCfg app)

app :: SpockM SqlBackend () () ()
app = do
  get "people" $ do
    allPeople <- runSQL $ selectList [] [Asc PersonId]
    json allPeople
  post "people" $ do
    maybePerson <- jsonBody :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> do
        setStatus badRequest400
        errorJson 1 "Failed to parse request body as Person"
      Just thePerson -> do
        newId <- runSQL $ insert thePerson
        setStatus created201
        json $ object ["result" .= String "success", "id" .= newId]
  get ("people" <//> var) $ \personId -> do
    maybePerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> do
        setStatus notFound404
        errorJson 2 "Could not find a person with matching id"
      Just thePerson -> json thePerson
  -- Solution to 1.
  put ("people" <//> var) $ \personId -> do
    maybePerson <- jsonBody
    case (maybePerson :: Maybe Person) of
      Nothing -> do
        setStatus badRequest400
        errorJson 1 "Failed to parse request body as Person"
      Just thePerson -> do
        runSQL $ replace personId thePerson
        setStatus created201
        json $ object ["result" .= String "success", "id" .= personId]
  delete ("people" <//> var) $ \personId -> do
    runSQL $ P.delete (personId :: PersonId)
    setStatus noContent204

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: MonadIO m => Int -> Text -> ActionCtxT ctx m ()
errorJson code message =
  json $
  object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

jsonErrorHandler :: Status -> ActionCtxT ctx IO ()
jsonErrorHandler (Status code message) = errorJson code (decodeUtf8 message)
