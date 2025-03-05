// Java Pogram to synchronized method by
// using an anonymous class
import java.io.*;

class Test {
    void test_func(int n)
    {
        // non synchronized method
        for (int i = 1; i <= 3; i++) {
            System.out.println(n + i);
            try {
                Thread.sleep(100);
            }
            catch (Exception e) {
                System.out.println(e);
            }
        }
    }
}

// Driver Class
public class ASyncPrintAsync {
    // Main function
    public static void main(String args[])
    {
        // only one object
        final Test O = new Test();

        Thread a = new Thread() {
            public void run() { O.test_func(15); }
        };

        Thread b = new Thread() {
            public void run() { O.test_func(30); }
        };

        a.start();
        b.start();
    }
}
