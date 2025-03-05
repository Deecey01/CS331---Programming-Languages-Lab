/**
 * SimpsonIntegration.java
 *
 * Usage:
 *   1) Compile: javac SimpsonIntegration.java
 *   2) Run:     java SimpsonIntegration <numThreads>
 *
 *      where <numThreads> is an integer, e.g. 4, 8, 16, etc.
 *
 * This program numerically approximates the integral
 *   ∫ from -1 to 1 of (1 / sqrt(2π)) * e^(-x^2 / 2) dx
 * using the Composite Simpson 1/3 Rule in a multithreaded way.
 */

import java.util.ArrayList;
import java.util.List;

public class SimpsonIntegration {

    // Lower bound of integral
    private static final double A = -1.0;
    // Upper bound of integral
    private static final double B =  1.0;
    // Number of subintervals (must be even). 
    // You can increase if you want more accuracy (e.g., 2_000_000).
    private static final int N = 1_000_000;

    // Function to integrate: f(x) = (1 / sqrt(2*pi)) * e^(-x^2 / 2)
    private static double f(double x) {
        return (1.0 / Math.sqrt(2 * Math.PI)) * Math.exp(- (x*x) / 2.0);
    }

    public static void main(String[] args) {
        // Parse number of threads from command line
        if (args.length < 1) {
            System.err.println("Usage: java SimpsonIntegration <numThreads>");
            return;
        }
        int numThreads = Integer.parseInt(args[0]);

        // Step size
        double h = (B - A) / N;

        // We will store partial sums from each thread here
        // The final Simpson’s rule formula is:
        // (h/3) * [ f(A) + 4 * sum(oddIndices) + 2 * sum(evenIndices) + f(B) ]
        // We'll break up the 'sum(oddIndices)' and 'sum(evenIndices)' among threads.
        double[] partialOddSums  = new double[numThreads];
        double[] partialEvenSums = new double[numThreads];

        // Thread class to compute partial sums
        class SimpsonThread extends Thread {
            int threadId;
            public SimpsonThread(int threadId) {
                this.threadId = threadId;
            }
            public void run() {
                // Determine the chunk of indices this thread will handle
                // We'll distribute the inner indices (1..N-1) across threads
                // Each index i corresponds to x_i = A + i*h
                int chunkSize = (N - 1) / numThreads;  // ignoring remainder for simplicity
                int start = 1 + threadId * chunkSize;
                int end   = (threadId == numThreads - 1) 
                            ? (N - 1)  // last thread goes to N-1 
                            : (start + chunkSize - 1);

                double localOddSum  = 0.0;
                double localEvenSum = 0.0;
                
                for (int i = start; i <= end; i++) {
                    double x_i = A + i*h;
                    if (i % 2 == 0) {
                        // even index
                        localEvenSum += f(x_i);
                    } else {
                        // odd index
                        localOddSum += f(x_i);
                    }
                }
                partialOddSums[threadId]  = localOddSum;
                partialEvenSums[threadId] = localEvenSum;
            }
        }

        // Create and start threads
        List<Thread> threads = new ArrayList<>();
        for (int t = 0; t < numThreads; t++) {
            Thread thr = new SimpsonThread(t);
            thr.start();
            threads.add(thr);
        }

        // Wait for all threads to finish
        for (Thread thr : threads) {
            try {
                thr.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        // Sum up the partial results
        double totalOddSum = 0.0;
        double totalEvenSum = 0.0;
        for (int t = 0; t < numThreads; t++) {
            totalOddSum  += partialOddSums[t];
            totalEvenSum += partialEvenSums[t];
        }

        // Apply Simpson's rule
        double fa = f(A);
        double fb = f(B);
        double result = (h / 3.0) * (fa + 4.0 * totalOddSum + 2.0 * totalEvenSum + fb);

        // Print the result
        System.out.println("Approximate integral value = " + result);
    }
}
