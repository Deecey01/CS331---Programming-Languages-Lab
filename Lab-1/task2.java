import java.util.Random;
// import java.util.concurrent.ExecutorService;
// import java.util.concurrent.Executors;
// import java.util.concurrent.TimeUnit;

public class task2 {
    private static int N = 1000; // Matrix size (N x N)
    private static double[][] A = new double[N][N]; // Matrix A
    private static double[][] B = new double[N][N]; // Matrix B
    private static double[][] C = new double[N][N]; // Matrix C (Resultant Matrix)
    
    // Compute a portion of the resultant matrix C
    private static void computeMatrixChunk(int startRow, int endRow) {
        for (int i = startRow; i < endRow; i++) { // Loop through assigned rows
            for (int j = 0; j < N; j++) { // Loop through all columns of the result matrix
                for (int k = 0; k < N; k++) { // Loop through columns of A and rows of B
                    C[i][j] += A[i][k] * B[k][j]; // Multiply and accumulate
                }
            }
        }
    }

    // Perform multithreaded matrix multiplication
    private static void multiplyMatrices(int n) {
        Thread[] threads = new Thread[n]; // Array to hold thread objects
    
        // Calculate the number of rows each thread will handle
        int rowsPerThread = (N + n - 1) / n;

        // ExecutorService executor = Executors.newFixedThreadPool(n);
    
        for (int id = 0; id < n; id++) {
            // Determine the range of rows this thread will process
            int startRow = id * rowsPerThread;
            int endRow = Math.min(startRow + rowsPerThread, N);
    
            // Create a new thread to compute the specified chunk of the matrix
            threads[id] = new Thread(new Runnable() {
                @Override
                public void run() {
                    computeMatrixChunk(startRow, endRow);
                }
            });
            threads[id].start(); // Start the thread
            // executor.submit(()->computeMatrixChunk(startRow, endRow));
        }

        // executor.shutdown();
        // try {
        //     executor.awaitTermination(Long.MAX_VALUE, TimeUnit.MILLISECONDS);
        // } catch (InterruptedException e) {
        //     e.printStackTrace();
        // }

    
        // Wait for all threads to complete
        for (Thread thread : threads) {
            try {
                thread.join(); // Wait for the thread to finish
            } catch (InterruptedException e) {
                e.printStackTrace(); // Handle interruption
            }
        }
    }
    
    // Print a subset of the matrix for verification (first size x size elements)
    private static void printMatrix(double[][] matrix, int size) {
        for (int i = 0; i < Math.min(size, N); i++) {
            for (int j = 0; j < Math.min(size, N); j++) {
                System.out.printf("%.2f ", matrix[i][j]); // Format to 2 decimal places
            }
            System.out.println(); // Newline after each row
        }
    }

    public static void main(String[] args) {
        // Check if the user provided the number of threads as an argument
        if (args.length != 1) {
            System.out.println("Usage: java task2 <number_of_threads>");
            return;
        }

        // Parse the number of threads from the command-line argument
        int n = Integer.parseInt(args[0]);

        // Validate that the number of threads is within a reasonable range
        if (n <= 0) {
            System.out.println("Number of threads must be greater than 0.");
            return;
        }

        // Measure initialization time
        long initStartTime = System.currentTimeMillis();

        // Initialize matrices A and B with random values
        Random cell_value = new Random();
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                A[i][j] = cell_value.nextDouble() * 10; // Random value for A[i][j]
                B[i][j] = cell_value.nextDouble() * 10; // Random value for B[i][j]
            }
        }

        long initEndTime = System.currentTimeMillis();
        long initTime = initEndTime - initStartTime; // Time taken for initialization

        // Measure multiplication time
        long multStartTime = System.currentTimeMillis();

        // Perform matrix multiplication using the specified number of threads
        multiplyMatrices(n);

        long multEndTime = System.currentTimeMillis();
        long multTime = multEndTime - multStartTime; // Time taken for multiplication

        // Output the results and time taken
        System.out.println("Matrix initialization completed.");
        System.out.println("Initialization time: " + initTime + " ms");
        System.out.println("Matrix multiplication completed with " + n + " threads.");
        System.out.println("Multiplication time: " + multTime + " ms");

        // Optionally, print a subset of the matrices for verification
        System.out.println("Matrix A:");
        printMatrix(A, 10); // Print the first 10x10 elements of Matrix A
        System.out.println("Matrix B:");
        printMatrix(B, 10); // Print the first 10x10 elements of Matrix B
        System.out.println("Matrix C (Result):");
        printMatrix(C, 10); // Print the first 10x10 elements of Matrix C
    }
}
