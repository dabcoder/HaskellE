import System.IO
import Data.Random

data Choice = Rock 
           | Paper 
           | Scissors
           | Error
  deriving Show

associate :: String -> Choice
associate s = case s of
    "1" -> Rock
    "2" -> Paper
    "3" -> Scissors
    _   -> Error

compareResults :: (Choice, Choice) -> String
compareResults (pc, cc) = case (pc, cc) of
    (Rock, Rock)      -> "It's a tie!"
    (Rock, Paper)     -> "You lose!"
    (Rock, Scissors)  -> "You win!"
    (Paper, Rock)     -> "You win!"
    (Paper, Paper)    -> "It's a tie!"
    (Paper, Scissors) -> "You lose!"
    (Scissors, Rock)  -> "You lose!"
    (Scissors, Paper) -> "You win!"
    (Scissors, Scissors) -> "It's a tie!"
    (Error, _ )          -> "Error"

main = do
    print "Choose 1: Rock, 2: Paper, 3: Scissors"
    -- player's choice
    choice <- getLine
    -- computer's choice
    compChoice <- sample $ randomElement  ["1", "2", "3"]
    -- associate choices
    let pc = associate choice
    let cc = associate compChoice
    -- print both choices
    print $ pc
    print $ cc
    -- print result
    print $ compareResults (pc, cc)