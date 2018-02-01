{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)
import Network.HTTP.Conduit

-- | Observation data
data Observation =
    Observation { compName          :: String  -- ^ The company's name
                , latestP           :: Float   -- ^ Latest stock price
                , latestT           :: String  -- ^ epoch timeframe
                } deriving (Show)

instance FromJSON Observation where
    parseJSON = withObject "Observation" $ \v -> Observation
        <$> v .: "companyName"
        <*> v .: "latestPrice"
        <*> v .: "latestTime"

-- builds the URL
conditionsQuery2 :: String -> String
conditionsQuery2 company =
    "https://api.iextrading.com/1.0/stock/" ++ company ++ "/quote"


main :: IO ()
main = do
    print "What's the company name?"
    company <- getLine
    response <- simpleHttp (conditionsQuery2 company)
    print $ (decode $ response :: Maybe Observation)
    