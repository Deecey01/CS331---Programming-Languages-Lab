/**
 * MatrixMultiplication.java
 *
 * Usage:
 *   1) Compile: javac MatrixMultiplication.java
 *   2) Run:     java MatrixMultiplication <numThreads>
 *
 *      where <numThreads> is an integer, e.g. 8, 10, 50, etc.
 *
 * This program:
 *   - Initializes two N=1000 matrices (A and B) with random values in [0..10].
 *   - Multiplies C = A x B in a multithreaded manner.
 *   - The number of threads is taken from the command line.
 */

import java.util.Random;
import java.util.ArrayList;
import java.util.List;

public class MatrixMultiplication {

    // Matrix size (N x N)
    private static final int N = 1000;
    private static final int MAX_RANDOM = 10; // random values in [0..10]

    // Shared matrices
    private static double[][] A;
    private static double[][] B;
    private static double[][] C;

    public static void main(String[] args) {
        if (args.length < 1) {
            System.err.println("Usage: java MatrixMultiplication <numThreads>");
            return;
        }
        int numThreads = Integer.parseInt(args[0]);

        // 1) Initialize A and B with random values, C with zeros
        A = new double[N][N];
        B = new double[N][N];
        C = new double[N][N]; // result matrix

        Random random = new Random();

        long initStartTime = System.currentTimeMillis();

        // Parallel initialization can also be done using multiple threads,
        // but for clarity we’ll do it sequentially or you can replicate the approach below.
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                A[i][j] = random.nextDouble() * MAX_RANDOM; 
                B[i][j] = random.nextDouble() * MAX_RANDOM;
            }
        }

        long initEndTime = System.currentTimeMillis();
        System.out.println("Initialization done in " + (initEndTime - initStartTime) + " ms.");

        // 2) Multithreaded matrix multiplication
        long mulStartTime = System.currentTimeMillis();

        // We create threads; each thread handles a chunk of rows
        List<Thread> threads = new ArrayList<>();
        int rowsPerThread = N / numThreads;
        
        for (int t = 0; t < numThreads; t++) {
            // Start row (inclusive)
            final int startRow = t * rowsPerThread;
            // End row (exclusive), except for the last thread
            final int endRow = (t == numThreads - 1) ? N : startRow + rowsPerThread;

            Thread worker = new Thread(() -> {
                for (int i = startRow; i < endRow; i++) {
                    for (int j = 0; j < N; j++) {
                        double sum = 0.0;
                        for (int k = 0; k < N; k++) {
                            sum += A[i][k] * B[k][j];
                        }
                        C[i][j] = sum;
                    }
                }
            });
            worker.start();
            threads.add(worker);
        }

        // Wait for all threads to finish
        for (Thread thr : threads) {
            try {
                thr.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        long mulEndTime = System.currentTimeMillis();
        System.out.println("Matrix multiplication done in " + (mulEndTime - mulStartTime) + " ms.");

        // 3) (Optional) We can verify one or two entries or print a small section
        // to confirm the result. But printing a 1000x1000 might be too large.

        // Example: print the sum of all elements in C (just to verify something)
        double totalSum = 0.0;
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                totalSum += C[i][j];
            }
        }
        System.out.println("Sum of all elements in C = " + totalSum);
    }
}
