
import java.util.concurrent.*;
 
public class BasicThread  {

    private static int part = 0;
 
    static class PrintThread implements Runnable {
 
        @Override
        public void run() {
            int thread_part = part++;
 	    char c= (char) (65+thread_part);
            for (int i = 0; i< 10; i++) {
                System.out.print(c);
            }
        }
    }
 
    // Driver Code
    public static void main(String[] args) throws InterruptedException {
 
        for(int i=0;i<args.length;i++)  System.out.println(args[i]); //parse   

        int MAX_THREAD =4;
       	MAX_THREAD=Integer.parseInt(args[0]); //First Argument : for running with four thread Use command : java BasicThread 4 	

        Thread[] threads = new Thread[MAX_THREAD];
	System.out.println("Number of thread created..="+MAX_THREAD);
 
        // Creating 4 threads
        for (int i = 0; i < MAX_THREAD; i++) {
            threads[i] = new Thread(new PrintThread());
            threads[i].start();
        }
 
        // Joining 4 threads i.e. waiting for all 4 threads to complete
        for (int i = 0; i < MAX_THREAD; i++) {
            threads[i].join();
        }
 
    }
}

