class Buffer {
    private final int[] buffer;
    private final int size;
    private int count;

    public Buffer(int size) {
        this.size = size;
        this.buffer = new int[size];
        this.count = 0;
    }

    public synchronized void produce(int item) throws InterruptedException {
        while (count == size) {
            // Buffer is full, wait for the consumer to consume
            wait();
        }

        buffer[count] = item;
        count++;
        System.out.println("Produced: " + item);

        // Notify the consumer that an item is available
        notify();
    }

    public synchronized int consume() throws InterruptedException {
        while (count == 0) {
            // Buffer is empty, wait for the producer to produce
            wait();
        }

        int item = buffer[count - 1];
        count--;
        System.out.println("Consumed: " + item);

        // Notify the producer that space is available
        notify();

        return item;
    }
}

class Producer implements Runnable {
    private final Buffer buffer;

    public Producer(Buffer buffer) {
        this.buffer = buffer;
    }

    @Override
    public void run() {
        try {
            for (int i = 1; i <= 5; i++) {
                buffer.produce(i);
                Thread.sleep(100);
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}

class Consumer implements Runnable {
    private final Buffer buffer;

    public Consumer(Buffer buffer) {
        this.buffer = buffer;
    }

    @Override
    public void run() {
        try {
            for (int i = 1; i <= 5; i++) {
                int item = buffer.consume();
                Thread.sleep(100);
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}

public class PC {
    public static void main(String[] args) {
        Buffer buffer = new Buffer(10);

        Thread producerThread = new Thread(new Producer(buffer));
        Thread consumerThread = new Thread(new Consumer(buffer));

        producerThread.start();
        consumerThread.start();

        try {
            producerThread.join();
            consumerThread.join();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
}
