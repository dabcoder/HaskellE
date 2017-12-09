toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther (x:[])   = [x]
doubleEveryOther (x:y:zs) = x : 2*y : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits []      = 0
sumDigits (x:[])  = x
sumDigits (x:xs)
    | n < 10    = x + sumDigits xs
    | otherwise = sumDigits (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate n = doubleEveryOther(toDigits n) 

main = do
    print $ toDigitsRev 5595 