# CS331 Assignment III - Haskell Programming

**Name:** Divyansh Chandak  
**Roll:** 220101039

This assignment implements three fundamental algorithms in Haskell:
1. Square Root calculation 
2. Fibonacci number calculation with O(n) complexity
3. QuickSort implementation using list comprehension

## How to Run the Program

### Method 1: Compiling with GHC

1. Save the code as `assignment3.hs`
2. Compile the program using GHC:
   ```
   ghc -o assignment3 assignment3.hs
   ```
3. Run the compiled executable:
   ```
   ./assignment3
   ```
4. Follow the interactive menu prompts to select and test different functions

### Method 2: Using GHCi Interactive Environment

1. Save the code as `assignment3.hs`
2. Start GHCi:
   ```
   ghci
   ```
3. Load the file:
   ```
   Prelude> :load assignment3.hs
   ```
4. You can now either:
   - Run the main program:
     ```
     *Main> main
     ```
   - Or call individual functions directly:
     ```
     *Main> squareRoot 23.56
     *Main> fibonacci 10
     *Main> quickSort [12, 2, 4, 5, 18]
     ```

## Function Descriptions

### A) Square Root Function

Implements the Newton-Raphson method to calculate square roots with an accuracy of 0.00001.

```haskell
squareRoot :: Double -> Double
```

#### Example Usage:
```
*Main> squareRoot 23.56
4.85386
```

### B) Fibonacci Function

Calculates the nth Fibonacci number with O(n) complexity using tail recursion.

```haskell
fibonacci :: Integer -> Integer
```

#### Example Usage:
```
*Main> fibonacci 10
55

*Main> fibonacci 200
280571172992510140037611932413038677189525
```

### C) QuickSort Implementation

Sorts a list of comparable elements using the QuickSort algorithm with list comprehension.

```haskell
quickSort :: Ord a => [a] -> [a]
```

#### Example Usage:
```
*Main> quickSort [12, 2, 4, 5, 18]
[2,4,5,12,18]
```

## Sample Program Interaction

When running the program interactively (through Method 1 or `main` in Method 2), you'll see a menu:

```
Welcome to CS331 Assignment III
Name: Divyansh Chandak
Roll: 220101039

==== CS331 Assignment III - Haskell Programming ====
1. Calculate Square Root
2. Calculate Fibonacci Number
3. Sort a List using QuickSort
4. Run Test Cases
0. Exit
=================================================
Enter your choice:
```

### Sample Input/Output for Option 1 (Square Root):
```
Enter your choice: 1

-- Square Root Calculation --
Enter a number: 
23.56
Square root of 23.56 is: 4.8538587171522155
Verification: 4.8538587171522155^2 = 23.56
```

### Sample Input/Output for Option 2 (Fibonacci):
```
Enter your choice: 2

-- Fibonacci Number Calculation --
Enter the position (n): 
10
Calculating fibonacci(10)...
The 10th Fibonacci number is: 55
```

### Sample Input/Output for Option 3 (QuickSort):
```
Enter your choice: 3

-- QuickSort List Sorting --
Enter a list of integers (space-separated): 
12 2 4 5 18
Original list: [12,2,4,5,18]
Sorted list: [2,4,5,12,18]
```

### Sample Output for Option 4 (Test Cases):
```
Enter your choice: 4

==== Running Test Cases ====

-- Square Root Test Cases --
squareRoot(23.56) = 4.8538587171522155 (expected ≈ 4.85386)
squareRoot(2.0) = 1.4142135623746899 (expected ≈ 1.41421)
squareRoot(9.0) = 3.0 (expected ≈ 3.0)
squareRoot(16.0) = 4.0 (expected ≈ 4.0)
squareRoot(100.0) = 10.0 (expected ≈ 10.0)

-- Fibonacci Test Cases --
fibonacci(0) = 0 (expected: 0)
fibonacci(1) = 1 (expected: 1)
fibonacci(5) = 5 (expected: 5)
fibonacci(10) = 55 (expected: 55)
fibonacci(20) = 6765 (expected: 6765)

Large Fibonacci Test:
fibonacci(200) = 
280571172992510140037611932413038677189525

-- QuickSort Test Cases --
quickSort([12,2,4,5,18]) = [2,4,5,12,18] (expected: [2,4,5,12,18])
quickSort([]) = [] (expected: [])
quickSort([5,4,3,2,1]) = [1,2,3,4,5] (expected: [1,2,3,4,5])
quickSort([3,1,4,1,5,9]) = [1,1,3,4,5,9] (expected: [1,1,3,4,5,9])
```

## Notes

- The program uses the Newton-Raphson method for square root calculation with 0.00001 accuracy
- The Fibonacci implementation uses tail recursion to achieve O(n) complexity
- Both Int and Integer types are available in Haskell, but this implementation uses Integer for Fibonacci to handle large numbers without overflow
- The QuickSort implementation uses Haskell's list comprehension for elegant and concise code