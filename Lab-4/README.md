# Binary Search Tree Traversal Assignment

## CS331: Programming Language Lab - Assignment IV
### Advanced Haskell Programming

This program implements a Binary Search Tree (BST) with various traversal methods as required by Assignment IV. The implementation includes pre-order, in-order, post-order, and breadth-first search (BFS) traversals.

## Features

- Parse comma-separated list of integers from user input
- Build a Binary Search Tree from the parsed integers
- Perform multiple traversals:
  - Pre-order traversal
  - In-order traversal
  - Post-order traversal
  - Breadth-First Search (BFS) traversal
- Embedded test cases for immediate verification
- Error handling for invalid inputs

## How to Run

1. Ensure you have GHC (Glasgow Haskell Compiler) installed on your system.
2. Save the code to a file named `220101039.hs`
3. Compile the program using:
   ```
   ghc 220101039.hs
   ```
4. Run the executable:
   ```
   ./220101039
   ```
   - On Windows: `bst_traversal.exe`

## Usage

1. When you run the program, it will automatically execute two embedded test cases:
   - `[4, 2, 9, 1, 5, 6, 3]`
   - `[5, 4, 21, 67, 6, 7, 1, 4]`

2. After displaying the test case results, the program will prompt you to enter your own list of numbers.

3. Enter a comma-separated list of integers (e.g., `10, 5, 15, 3, 7, 12, 20`).

4. The program will display:
   - The parsed input
   - Pre-order traversal result
   - In-order traversal result
   - Post-order traversal result
   - Breadth-First Search (BFS) traversal result

## Input Restrictions

- The program accepts only integers as input.
- Inputs must be comma-separated.
- If any non-numeric characters are entered (except commas and whitespace), the program will display a warning message: "Warning: Invalid numbers found in input."
- Empty inputs will also trigger the warning.

## Implementation Details

- The program uses a custom parser that leverages monadic error handling to process user input.
- The BST is implemented with a standard node structure containing a value and left/right subtrees.
- Traversal algorithms are implemented using pure recursive functions.
- Built-in Haskell libraries are used for certain functions:
  - `Control.Monad` for monadic operations
  - `Data.Char` for character classification
  - `Data.List` for list manipulation
  - `Text.Read` and `Data.Maybe` for parsing support

## Test Cases

The code includes two pre-defined test cases that run automatically:

1. `[4, 2, 9, 1, 5, 6, 3]` - Creates a balanced BST with multiple levels
2. `[5, 4, 21, 67, 6, 7, 1, 4]` - Tests handling of duplicate values and creates a more complex tree structure

The program will show the traversal results for both test cases before asking for user input.

## Error Handling

The program includes robust error handling for parsing user input. It will gracefully handle:
- Empty input
- Non-numeric input
- Extra commas
- Leading/trailing whitespace