{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics

data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Generic, Show)

instance ToJSON Person

instance FromJSON Person

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: SpockM () () () ()
app = do
  get "person" $ do
    json [Person { name = "Fry", age = 25 }, Person { name = "Bender", age = 4 }]
  post "person" $ do
    maybePerson <- jsonBody
    case (maybePerson :: Maybe Person) of
      Nothing -> text "Failed to parse request body as Person"
      Just thePerson  -> text $ "Parsed: " <> pack (show thePerson)
