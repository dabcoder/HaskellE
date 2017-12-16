fizzRange :: [Integer]
fizzRange = [1..20]

fizzBuzz :: Integer -> String
fizzBuzz n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 5  == 0 = "Buzz"
    | n `mod` 3  == 0 = "Fizz"
    | otherwise       = show n

main = do
    print $ map fizzBuzz(fizzRange)