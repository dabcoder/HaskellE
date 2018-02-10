{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import GHC.Generics
import Data.ByteString.Lazy.Char8 (pack)
import Network.HTTP.Conduit


-- | Observation data
data Monitor =
    Monitor { name             :: String 
            , query            :: String  
            , created          :: String
            , modified         :: String
            } deriving (Show, Generic)

instance FromJSON Monitor where
    parseJSON = withObject "Monitor" $ \v -> Monitor
        <$> v .: "name"
        <*> v .: "query"
        <*> v .: "created"
        <*> v .: "modified"


api_key, app_key :: String
api_key = "<api_key>"
app_key = "<app_key>"


-- GET all monitors URL
getAllMonitorsQuery :: String -> String -> String
getAllMonitorsQuery apikey appkey =
    "https://app.datadoghq.com/api/v1/monitor?api_key=" ++ apikey ++ "&application_key=" ++ appkey


main :: IO ()
main = do
    response <- simpleHttp (getAllMonitorsQuery api_key app_key)
    let r = decode response :: Maybe [Monitor]
    print $ r
