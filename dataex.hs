-- Data section
-- /show
data FailableDouble = Failure 
                    | OK Double
  deriving Show

-- /show
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

-- show
-- Store a person's name, age, and favorite Thing
data Person = Person String Int Thing
  deriving Show

-- Function section
-- show
safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

-- show
failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK x) = x

-- show
failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
  Failure -> 0
  OK d    -> d
  
brent :: Person
brent = Person "Brent" 30 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

david :: Person
david = Person "David" 33 King

getName :: Person -> String
getName (Person n _ _) = n

getAge :: Person -> Int
getAge (Person _ a _) = a

-- show
baz :: Person -> String
baz p@(Person n _ d) = "The name field of (" ++ show p ++ ") is " ++ n ++ "and its data type: " ++ show d

-- show
checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n 33 _)         = n ++ ", you're not young anymore!"
checkFav (Person n _ _)          = n ++ ", your favorite thing is lame."

main = do
    -- Failure
    print (safeDiv 2 0, safeDiv 3 4)
    print (failureToZero Failure, failureToZero (OK 3.4))
    print (failureToZero' Failure, failureToZero' (OK 3.4))
    -- Person
    print (getName david)
    print (getAge brent)
    print (getAge stan)
    putStrLn (baz brent)
    putStrLn (checkFav david)