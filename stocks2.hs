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
data Observation =
    Observation { company          :: String  -- ^ The company's name
                , latestPrice      :: Float   -- ^ Latest stock price
                , latestTime       :: String  -- ^ epoch timeframe
                } deriving (Show, Generic)

instance FromJSON Observation where
    parseJSON = withObject "Observation" $ \v -> Observation
        <$> v .: "companyName"
        <*> v .: "latestPrice"
        <*> v .: "latestTime"

-- builds the URL
conditionsQuery :: String -> String
conditionsQuery company =
    "https://api.iextrading.com/1.0/stock/" ++ company ++ "/quote"


main :: IO ()
main = do
    print "What's the company name?"
    comp <- getLine
    response <- simpleHttp (conditionsQuery comp)
    let r1 = decode response :: Maybe Observation
    case r1 of 
        Nothing                -> putStrLn "No data"
        Just (Observation{..}) -> do
            putStrLn $ "The latest stock price of " ++ company ++ " is " ++ show latestPrice
            putStrLn $ "Recorded today at: " ++ latestTime
