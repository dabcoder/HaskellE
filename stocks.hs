{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.Scientific as Scientific

import Data.Aeson.Lens (_String, key)
import Data.Aeson.Lens (_Number, key)
import Network.Wreq

stockWorthBuying :: Scientific -> String
stockWorthBuying n 
    | n <= 60 = "Worth buying perhaps?"
    | n > 60  = "Too expensive!"

-- builds the URL
conditionsQuery2 :: String -> String
conditionsQuery2 company =
    "https://api.iextrading.com/1.0/stock/" ++ company ++ "/quote"

-- User input for the company 
main :: IO()
main = do
    print "What's the company name?"
    company <- getLine
    r <- get (conditionsQuery2 company)
    putStrLn "The full company's name is: "
    print $ r ^. responseBody . key "companyName" . _String 
    putStrLn "And the stock price is: "
    print $ r ^?! responseBody . key "latestPrice" . _Number
    let a = r ^?! responseBody . key "latestPrice" . _Number
    putStrLn $ stockWorthBuying a

