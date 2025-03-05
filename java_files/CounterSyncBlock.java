// Java Program to demonstrate synchronization block in Java

class Counter {
    private int c = 0; // Shared variable

    // Method with synchronization block
    public void inc() {
        synchronized(this) { // Synchronize only this block
            c++;
        }
    }

    // Method to get counter value
    public int get() {
        return c;
    }
}

public class CounterSyncBlock {
    public static void main(String[] args) {
        Counter cnt = new Counter(); // Shared resource

        // Thread 1 to increment counter
        Thread t1 = new Thread(() -> {
            for (int i = 0; i < 1000; i++) {
                cnt.inc();
            }
        });

        // Thread 2 to increment counter
        Thread t2 = new Thread(() -> {
            for (int i = 0; i < 1000; i++) {
                cnt.inc();
            }
        });

        // Start both threads
        t1.start();
        t2.start();

        // Wait for threads to finish
        try {
            t1.join();
            t2.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        // Print final counter value
        System.out.println("Counter: " + cnt.get());
    }
}
