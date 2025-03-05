// A Java program to demonstrate working of synchronized.
import java.io.*;
// A Class used to send a message
class Sender {
    public void send(String msg)
    {
        System.out.println("Sending " + msg);  // Changed to print without new line
        try {
            Thread.sleep(100);
        }
        catch (Exception e) {
            System.out.println("Thread  interrupted.");
        }
        System.out.println(msg + "Sent");  // Improved output format
    }
}

// Class for sending a message using Threads
class ThreadedSend extends Thread {
    private String msg;
    Sender sender;

    // Receives a message object and a string message to be sent
    ThreadedSend(String m, Sender obj)
    {
        msg = m;
        sender = obj;
    }

    public void run()
    {
        // Only one thread can send a message at a time.
        synchronized (sender)
        {
            // Synchronizing the send object
            sender.send(msg);
        }
    }
}

// Driver class
class SyncMessg {
    public static void main(String args[])
    {
        Sender send = new Sender();
        ThreadedSend S1 = new ThreadedSend("Hi ", send);
        ThreadedSend S2 = new ThreadedSend("Bye ", send);

        // Start two threads of ThreadedSend type
        S1.start();
        S2.start();

        // Wait for threads to end
        try {
            S1.join();
            S2.join();
        }
        catch (Exception e) {
            System.out.println("Interrupted");
        }
    }
}
