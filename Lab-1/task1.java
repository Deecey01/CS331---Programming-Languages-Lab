import java.util.concurrent.ExecutorService;
// Purpose: It is an interface that represents a pool of threads designed to execute submitted tasks.
// Key Features:
// Allows you to manage and control a group of threads.
// Abstracts away the complexity of thread creation, management, and termination.
// Provides methods like:
// submit(): Submits a task for execution and returns a Future.
// shutdown(): Gracefully shuts down the executor (it finishes all ongoing tasks before termination).
// awaitTermination(): Waits for all tasks to complete or for a specified timeout.

import java.util.concurrent.Executors;
// Purpose: This is a utility class that provides factory methods to create and manage thread pools (which are implementations of ExecutorService).
// Key Features:
// Provides factory methods for creating different types of thread pools:
// newFixedThreadPool(int nThreads): Creates a thread pool with a fixed number of threads.
// newCachedThreadPool(): Creates a thread pool that can dynamically adjust the number of threads based on demand.
// newSingleThreadExecutor(): Creates a single-threaded executor.
// newScheduledThreadPool(int nThreads): Creates a thread pool for scheduling tasks at fixed rates or with delays.


import java.util.concurrent.Future;
// Purpose: It represents the result of an asynchronous computation.
// Key Features:
// Acts as a placeholder for a value that will be available in the future after a task completes.
// Provides methods to:
// get(): Retrieves the result of the computation (blocks until the result is ready).
// cancel(boolean mayInterruptIfRunning): Attempts to cancel the task.
// isDone(): Checks if the task has completed.
// isCancelled(): Checks if the task was canceled.

public class task1 {
    public static void main(String[] args) throws Exception {
        // Ensure the program is run with one command-line argument specifying the number of threads
        if (args.length != 1) {
            System.out.println("Usage: java task1 <number_of_threads>");
            return;
        }

        // Parse the number of threads from the command-line argument
        int numberOfThreads = Integer.parseInt(args[0]);

        // Validate the number of threads is greater than 0
        if (numberOfThreads <= 0) {
            System.out.println("Number of threads must be greater than 0.");
            return;
        }

        // Number of points to divide the interval into (must be even for Simpson's rule)
        int n = 1_000_002;
        // Start and end points of the integration interval
        double a = -1.0, b = 1.0;
        // Step size for each interval
        double delta_x = (b - a) / n;

        // Print configuration details
        System.out.println("Number of threads: " + numberOfThreads);
        System.out.println("Total points: " + n);

        // Create a thread pool with the specified number of threads
        ExecutorService executor = Executors.newFixedThreadPool(numberOfThreads);
        @SuppressWarnings("unchecked")
        Future<Double>[] results = (Future<Double>[]) new Future<?>[numberOfThreads];

        // Divide the total points among the threads
        int pointsPerThread = n / numberOfThreads;

        // Record the start time of the computation
        long startTime = System.nanoTime();

        // Assign tasks to each thread
        for (int i = 0; i < numberOfThreads; i++) {
            // Define the range of points for the current thread
            int start = i * pointsPerThread;
            int end = (i == numberOfThreads - 1) ? n : start + pointsPerThread;
            // Submit the computation task for this range to the executor
            results[i] = executor.submit(() -> computePartialSum(a, delta_x, start, end));
        }

        // Aggregate results from all threads
        double integral_value = 0.0;
        for (Future<Double> result : results) {
            integral_value += result.get(); // Wait for and retrieve each thread's result
        }

        // Apply the Simpson's rule formula
        integral_value = (delta_x / 3) * integral_value;

        // Shut down the executor service
        executor.shutdown();

        // Record the end time of the computation
        long endTime = System.nanoTime();
        long duration = endTime - startTime;

        // Print the final approximated integral value and the time taken
        System.out.println("Approximated integral_value value: " + integral_value);
        System.out.println("Time taken (ms): " + (duration / 1_000_002.0));
    }

    // Function to compute a partial sum for a given range of points
    private static double computePartialSum(double a, double h, int start, int end) {
        double sum = 0.0;
        for (int i = start; i < end; i++) {
            // Calculate the x-coordinate for the current point
            double x = a + i * h;
            // Determine the coefficient for Simpson's rule (1 for endpoints, 4 for odd indices, 2 for even indices)
            double coeff = (i == 0 || i == end - 1) ? 1 : (i % 2 == 0 ? 2 : 4);
            // Add the weighted function value to the sum
            sum += coeff * calculate(x);
        }
        return sum;
    }

    // Function to evaluate the mathematical function at a given x-coordinate
    private static double calculate(double x) {
        // Evaluate the function 1 / sqrt(2 * PI) * exp(-x^2 / 2)
        return (1 / Math.sqrt(2 * Math.PI)) * Math.exp(-x * x / 2);
    }
}
