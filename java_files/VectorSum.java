
import java.util.concurrent.*;
 
public class VectorSum  {
 
    // Size of array
    private static final int MAX = 16;
 
    // Maximum number of threads
    private static final int MAX_THREAD = 4;
 
    private static int[] a = { 1, 5, 7, 10, 12, 14, 15, 18, 20, 22, 25, 27, 30, 64, 110, 220 };
    private static int[] sum = new int[MAX_THREAD];
    private static int part = 0;
 
    static class SumArray implements Runnable {
 
        @Override
        public void run() {
 
            // Each thread computes sum of 1/4th of array
            int thread_part = part++;
 
            for (int i = thread_part * (MAX / 4); i < (thread_part + 1) * (MAX / 4); i++) {
                sum[thread_part] += a[i];
            }
        }
    }
 
    // Driver Code
    public static void main(String[] args) throws InterruptedException {
 
        Thread[] threads = new Thread[MAX_THREAD];
 
        // Creating 4 threads
        for (int i = 0; i < MAX_THREAD; i++) {
            threads[i] = new Thread(new SumArray());
            threads[i].start();
        }
 
        // Joining 4 threads i.e. waiting for all 4 threads to complete
        for (int i = 0; i < MAX_THREAD; i++) {
            threads[i].join();
        }
 
        // Adding sum of all 4 parts
        int total_sum = 0;
        for (int i = 0; i < MAX_THREAD; i++) {
            total_sum += sum[i];
        }
 
        System.out.println("sum is " + total_sum);
    }
}

