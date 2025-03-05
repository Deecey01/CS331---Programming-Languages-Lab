# TASK-1 : Simpson Integration using Multithreading

## Overview
This Java program demonstrates the approximation of an integral using the Composite Simpson's 1/3 Rule. The program calculates the value of the integral. The program uses multithreading to divide the computation among multiple threads, improving efficiency. The number of threads is passed as a command-line argument.


## Features
- Approximates the integral using Simpson's 1/3 Rule.
- Utilizes multithreading for improved performance.
- Supports a large number of points 10^6 for increased accuracy.
- Allows varying the number of threads (between 4 and 16).

## How to Compile and Run

### Prerequisites
- Java Development Kit (JDK) installed.
- Ensure the Java compiler (`javac`) and runtime (`java`) are accessible via the command line.

### Compilation
1. Save the program in a file named `task1.java`.
2. Open a terminal/command prompt.
3. Navigate to the directory containing the file.
4. Compile the program:
   ```bash
   javac task1.java
   ```

### Execution
1. Run the program with the desired number of threads as a command-line argument:
   ```bash
   java task1 <numThreads>
   ```
   Replace `<numThreads>` with the number of threads (between 4 and 16).

### Example
To run the program with 8 threads:
```bash
java task1 2000
```

## Output
The program outputs the approximate value of the integral along with the computation time.

Example output:
```
Number of threads: 2000
Total points: 1000002
Approximated integral value: 0.6813239538557478
Time taken (ms): 252.6354
```
## Important Constraints
- The number of threads must be between 4 and 16.
- If an invalid number of threads is provided, the program displays an error message and exits.

### Example Error
For invalid thread input:
```bash
java task1 2
```
Output:
```
Number of threads must be between 4 and 16
```

## How Multithreading is Implemented
- The interval [-1, 1] is divided into subintervals based on the total number of points.
- Each thread is responsible for computing the Simpson's Rule for its assigned subintervals.
- The results from all threads are combined to produce the final integral value.

### Thread Logic
1. Each thread computes a partial sum for its subintervals.
2. The main thread aggregates the results from all threads.


## Performance Analysis
- Increasing the number of threads can reduce computation time by distributing work across CPU cores.
- Using too many threads may introduce overhead, especially for small problem sizes.


## Notes
- The program assumes that the number of points is large 10^6 for accurate results.
- Ensure the number of points is divisible by the number of threads to avoid uneven workloads.
- The exact value of the integral is approximately 0.682689492.


# TASK-2 : Matrix Multiplication using Multithreading

## Overview
This Java program demonstrates multithreaded matrix multiplication for two square matrices (A and B) of size N x N, where N = 1000 The program also initializes the matrices with random values between 0 and 10 and computes the product matrix (C) using multiple threads. The number of threads is specified as a command-line argument.

## Features
- Random initialization of matrices A and B.
- Multithreaded computation of matrix multiplication.
- Support for varying numbers of threads: 8, 10, 50, 100, and 500.
- Displays the three matrices (A, B, and C).

## How to Compile and Run

### Prerequisites
- Java Development Kit (JDK) installed.
- Ensure the Java compiler (`javac`) and runtime (`java`) are accessible via the command line.

### Compilation
1. Save the program in a file named `task1.java`.
2. Open a terminal/command prompt.
3. Navigate to the directory containing the file.
4. Compile the program:
   ```bash
   javac task1.java
   ```

### Execution
1. Run the program with the desired number of threads as a command-line argument:
   ```bash
   java task1 <numThreads>
   ```
   Replace `<numThreads>` with the desired number of threads (e.g., 8, 10, 50, 100, or 500).

### Example
To run the program with 10 threads:
```bash
java task1 10
```

## Output
1. The program will display matrices A, B, and C after the computation.
2. Example output:

```
Matrix A:
[1, 3, 2, ...]
[0, 7, 5, ...]
...

Matrix B:
[9, 0, 4, ...]
[2, 6, 8, ...]
...

Matrix C:
[29, 33, 40, ...]
[74, 95, 88, ...]
...
```

## Important Constraints
- The program checks if the number of threads is within a valid range (1 to \( N \)).
- If an invalid number of threads is provided, the program displays an error message and exits.

### Example Error
For invalid thread input:
```bash
java task1 0
```
Output:
```
Number of threads must be between 1 and 1000
```

## How Multithreading is Implemented
- The total number of rows N is divided among the threads.
- Each thread is responsible for computing a subset of rows in the resulting matrix C.
- The workload is evenly distributed whenever possible.

### Thread Logic
1. Each thread calculates the matrix product for a specific range of rows.
2. The range is determined by dividing N by the number of threads.

## Performance Analysis
- Increasing the number of threads can speed up the computation by distributing work across CPU cores.
- However, excessive threads (e.g., more than the number of rows or available cores) may introduce overhead and reduce efficiency.


## Notes
- Random numbers are generated using `java.util.Random`.
- The program prints the entire matrices for small-scale testing and visualization. For practical use, consider limiting or suppressing the output for large N x N matrices.
