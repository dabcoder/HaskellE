fizzRange :: [Integer]
fizzRange = [1..20]

fizzBuzz :: Integer -> String
fizzBuzz n
    | n `mod` 3  == 0 = "Fizz"
    | n `mod` 5  == 0 = "Buzz"
    | n `mod` 15 == 0 = "FizzBuzz"
    | otherwise       = show n

main = do
    print $ map fizzBuzz(fizzRange)
