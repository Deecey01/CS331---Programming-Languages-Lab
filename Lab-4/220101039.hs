module Main where

import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.Char (isDigit, isSpace)
import qualified Data.List as L
import Control.Monad (foldM)


-- Binary Search Tree Definition
data BST a = Empty | Node a (BST a) (BST a) deriving (Show)

-- Function to insert a value into the BST
insertBST :: Ord a => a -> BST a -> BST a
insertBST x Empty = Node x Empty Empty
insertBST x (Node v left right)
    | x <= v    = Node v (insertBST x left) right
    | otherwise = Node v left (insertBST x right)

-- Pre-order Traversal
preorderTraversal :: BST a -> [a]
preorderTraversal Empty = []
preorderTraversal (Node v left right) = 
    [v] ++ preorderTraversal left ++ preorderTraversal right

-- In-order Traversal
inorderTraversal :: BST a -> [a]
inorderTraversal Empty = []
inorderTraversal (Node v left right) = 
    inorderTraversal left ++ [v] ++ inorderTraversal right

-- Post-order Traversal
postorderTraversal :: BST a -> [a]
postorderTraversal Empty = []
postorderTraversal (Node v left right) = 
    postorderTraversal left ++ postorderTraversal right ++ [v]

-- Breadth-First Search (BFS) Traversal
bfsTraversal :: BST a -> [a]
bfsTraversal tree = bfs [tree]
    where 
        bfs [] = []
        bfs nodes = 
            let currentLevelNodes = [v | Node v _ _ <- nodes]
                nextLevelNodes = concat [getChildren node | node <- nodes]
            in currentLevelNodes ++ bfs nextLevelNodes
        
        getChildren Empty = []
        getChildren (Node _ left right) = [left, right]

-- Build BST from a list of numbers
buildBST :: [Int] -> BST Int
buildBST [] = Empty    
buildBST (h:t) = ctree2 (Node h Empty Empty) t
    where
        ctree2 tr [] = tr
        ctree2 tr (h:t) = ctree2 (insertBST h tr) t

-- Custom parser for comma-separated input
parseNumericInput :: String -> [Int]
parseNumericInput input = case readNumbersFromText input of
    Left err -> []  -- Handle parse errors (could log error message)
    Right nums -> nums
  where
    readNumbersFromText :: String -> Either String [Int]
    readNumbersFromText = foldM addNumber [] . tokenize

    tokenize :: String -> [String]
    tokenize = filter (not . null) . splitAtCommas . L.dropWhileEnd isSpace . dropWhile isSpace
    
    splitAtCommas :: String -> [String]
    splitAtCommas "" = []
    splitAtCommas str = 
        let (token, rest) = span (/= ',') str
            remainder = dropWhile (== ',') (drop 1 rest)
        in token : splitAtCommas remainder
    
    addNumber :: [Int] -> String -> Either String [Int]
    addNumber acc numStr = 
        case reads (L.dropWhileEnd isSpace $ dropWhile isSpace numStr) of
            [(num, "")] -> Right (acc ++ [num])
            _           -> Left $ "Invalid number: " ++ numStr

-- Pretty formatting for output lists
formatList :: Show a => [a] -> String
formatList [] = "[]"
formatList xs = "[" ++ L.intercalate ", " (map show xs) ++ "]"

-- Test case function to demonstrate all operations with a sample input
runTestCase :: [Int] -> IO ()
runTestCase testNumbers = do
    putStrLn "\n==== Test Case Execution ===="
    putStrLn $ "Test Input: " ++ formatList testNumbers
    
    let testTree = buildBST testNumbers
    
    putStrLn "\nTraversal Results for Test Case:"
    putStrLn $ "Pre-order:    " ++ formatList (preorderTraversal testTree)
    putStrLn $ "In-order:     " ++ formatList (inorderTraversal testTree)
    putStrLn $ "Post-order:   " ++ formatList (postorderTraversal testTree)
    putStrLn $ "Breadth-First Search(BFS):  " ++ formatList (bfsTraversal testTree)
    putStrLn "\nTest case execution completed successfully."
    putStrLn "=============================="

-- Main function to handle input and output
main :: IO ()
main = do
    putStrLn "Welcome to CS331 Assignment IV"
    putStrLn "Name: Divyansh Chandak"
    putStrLn "Roll: 220101039"
    putStrLn "\n==== Binary Search Tree Implementation ===="

    -- Run the embedded test case first
    let testCase = [4, 2, 9, 1, 5, 6, 3]
    runTestCase testCase
    let testCase = [5,4,21,67,6,7,1,4]
    runTestCase testCase

    
    putStrLn "\n\nEnter a list of numbers separated by commas:"
    input <- getLine
    let numbers = parseNumericInput input
    
    if null numbers
        then putStrLn "Warning: Invalid numbers found in input."
        else do
            putStrLn $ "\nParsed input: " ++ formatList numbers
            
            let tree = buildBST numbers
            
            putStrLn "\n=== Tree Traversal Results ==="
            putStrLn $ "Pre-order:   " ++ formatList (preorderTraversal tree)
            putStrLn $ "In-order:    " ++ formatList (inorderTraversal tree)
            putStrLn $ "Post-order:  " ++ formatList (postorderTraversal tree)
            putStrLn $ "Breadth-First Search (BFS): " ++ formatList (bfsTraversal tree)