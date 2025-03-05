strlen :: IO ()
strlen  = do putStr "Enter a string: "
             xs <- getLine
             putStr "The string has "
             putStr (show (length xs))
             putStrLn " characters"

-- data Person = Person String String Int Float String String deriving (Show)
-- -- let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-- firstName (Person firstname _ _ _ _ _) = firstname
-- lastName (Person _ lastname _ _ _ _) = lastname
-- age (Person _ _ age _ _ _) = age
-- height (Person _ _ _ height _ _) = height
-- phoneNumber (Person _ _ _ _ number _) = number
-- flavor (Person _ _ _ _ _ flavor) = flavor

data Person2 = Person2 { firstName :: String, lastName :: String, age :: Int , height :: Float , phoneNumber :: String , flavor :: String } deriving (Show)

data Car a b c = Car { company :: a, model :: b, year :: c } deriving (Show)
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " "++m++" was made in "++ show y

data Vector a = Vector a a a deriving (Show)
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n