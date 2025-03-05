-- Name: Divyansh Chandak
-- Roll: 220101039

-- A) Square Root function with accuracy 0.00001 
newtonRaphson :: Double -> Double -> Double -> Double
newtonRaphson n guess epsilon
    | isAccurateEnough = guess
    | otherwise = newtonRaphson n nextGuess epsilon
    where
        isAccurateEnough = abs (guess * guess - n) <= epsilon
        nextGuess = (guess + n / guess) / 2.0

squareRoot :: Double -> Double
squareRoot n = newtonRaphson n 1.0 0.00001


-- B) Fibonacci function with O(n) complexity
fibonacci :: Integer -> Integer
fibonacci n = fib n 0 1
  where
    fib :: Integer -> Integer -> Integer -> Integer
    fib 0 a _ = a
    fib n a b = fib (n-1) b (a+b)


-- C) QuickSort implementation using list comprehension
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort ys ++ [x] ++ quickSort zs
    where
        ys = [a | a <- xs, a <= x]
        zs = [b | b <- xs, b > x]

-- Helper function to display menu
displayMenu :: IO ()
displayMenu = do
    putStrLn "\n==== CS331 Assignment III - Haskell Programming ===="
    putStrLn "1. Calculate Square Root"
    putStrLn "2. Calculate Fibonacci Number"
    putStrLn "3. Sort a List using QuickSort"
    putStrLn "4. Run Test Cases"
    putStrLn "0. Exit"
    putStrLn "================================================="
    putStrLn "Enter your choice: "

-- Helper function to run square root calculation
runSquareRoot :: IO ()
runSquareRoot = do
    putStrLn "\n-- Square Root Calculation --"
    putStrLn "Enter a number: "
    input <- getLine
    let number = read input :: Double
    let result = squareRoot number
    putStrLn $ "Square root of " ++ show number ++ " is: " ++ show result
    putStrLn $ "Verification: " ++ show result ++ "^2 = " ++ show (result * result)

-- Helper function to run fibonacci calculation
runFibonacci :: IO ()
runFibonacci = do
    putStrLn "\n-- Fibonacci Number Calculation --"
    putStrLn "Enter the position (n): "
    input <- getLine
    let n = read input :: Integer
    putStrLn $ "Calculating fibonacci(" ++ show n ++ ")..."
    let result = fibonacci n
    putStrLn $ "The " ++ show n ++ "th Fibonacci number is: " ++ show result

-- Helper function to run quicksort
runQuickSort :: IO ()
runQuickSort = do
    putStrLn "\n-- QuickSort List Sorting --"
    putStrLn "Enter a list of integers (space-separated): "
    input <- getLine
    let numbers = map read (words input) :: [Int]
    putStrLn $ "Original list: " ++ show numbers
    let sorted = quickSort numbers
    putStrLn $ "Sorted list: " ++ show sorted

-- Helper function to run all test cases
runTestCases :: IO ()
runTestCases = do
    putStrLn "\n==== Running Test Cases ===="
    
    -- Square Root test cases
    putStrLn "\n-- Square Root Test Cases --"
    let sqrtTests = [(23.56, 4.85386), (2, 1.41421), (9, 3.0), (16, 4.0), (100, 10.0)]
    mapM_ (\(input, expected) -> do
        let result = squareRoot input
        putStrLn $ "squareRoot(" ++ show input ++ ") = " ++ show result ++ 
                   " (expected ≈ " ++ show expected ++ ")")
        sqrtTests
    
    -- Fibonacci test cases
    putStrLn "\n-- Fibonacci Test Cases --"
    let fibTests = [(0, 0), (1, 1), (5, 5), (10, 55), (20, 6765)]
    mapM_ (\(input, expected) -> do
        let result = fibonacci input
        putStrLn $ "fibonacci(" ++ show input ++ ") = " ++ show result ++ 
                   " (expected: " ++ show expected ++ ")")
        fibTests
    
    -- Special test case for Fibonacci(200)
    putStrLn "\nLarge Fibonacci Test:"
    putStrLn "fibonacci(200) = "
    print $ fibonacci 200
    
    -- QuickSort test cases
    putStrLn "\n-- QuickSort Test Cases --"
    let sortTests = [
            ([12, 2, 4, 5, 18], [2, 4, 5, 12, 18]),
            ([], []),
            ([5, 4, 3, 2, 1], [1, 2, 3, 4, 5]),
            ([3, 1, 4, 1, 5, 9], [1, 1, 3, 4, 5, 9])
            ]
    mapM_ (\(input, expected) -> do
        let result = quickSort input
        putStrLn $ "quickSort(" ++ show input ++ ") = " ++ show result ++ 
                   " (expected: " ++ show expected ++ ")")
        sortTests

-- Main function with interactive menu
main :: IO ()
main = do
    putStrLn "Welcome to CS331 Assignment III"
    putStrLn "Name: Divyansh Chandak"
    putStrLn "Roll: 220101039"
    
    -- Start the menu loop
    menuLoop
    
    where
        menuLoop = do
            displayMenu
            choice <- getLine
            case choice of
                "1" -> runSquareRoot >> menuLoop
                "2" -> runFibonacci >> menuLoop
                "3" -> runQuickSort >> menuLoop
                "4" -> runTestCases >> menuLoop
                "0" -> putStrLn "Thank you for using the program. Goodbye!"
                _   -> putStrLn "Invalid choice. Please try again." >> menuLoop

-- Examples of individual function usage:
-- > squareRoot 23.56
-- 4.85386
--
-- > fibonacci 10
-- 55
--
-- > fibonacci 200
-- 280571172992510140037611932413038677189525
--
-- > quickSort [12, 2, 4, 5, 18]
-- [2,4,5,12,18]