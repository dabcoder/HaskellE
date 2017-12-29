import System.IO
import Data.Random

data Choice = Rock 
           | Paper 
           | Scissors 
  deriving Show

associate :: String -> Choice
associate s = case s of
    "1" -> Rock
    "2" -> Paper
    "3" -> Scissors

main = do
    print "Hit 1: Rock, 2: Paper, 3: Scissors"
    choice <- getLine
    -- Ask again if choice is not either 1, 2 or 3
    
    -- Print the player's choice
    --putStrLn $ "You chose " ++ choice ++ "!"

    -- computer choice
    compChoice <- sample $ randomElement  ["1", "2", "3"]
    print $ associate choice
    print $ associate compChoice


