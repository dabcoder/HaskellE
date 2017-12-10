-- "Convert" an integer to a list
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- Reverse that list
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Double every other number in a list
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther (x:[])   = [x]
doubleEveryOther (x:y:zs) = x : 2*y : doubleEveryOther zs

-- Sum every single digits of a list
sumDigits :: [Integer] -> Integer
sumDigits []      = 0
sumDigits (x:[])  = x
sumDigits (x:xs)
    | x < 10    = x + sumDigits xs
    | otherwise = sumDigits (toDigits x) + sumDigits xs

-- Use those 4 above functions to validate the CC number
validate :: Integer -> Bool
validate n 
    | sumDigits (doubleEveryOther(toDigits n)) `mod` 10 == 0 = True
    | otherwise                                              = False

-- Example
main = do
    print $ validate 5595493143567890 -- returns False