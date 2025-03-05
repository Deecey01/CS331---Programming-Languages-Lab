


public class MainExtend extends Thread {
  public static void main(String[] args) {
    MainExtend thread = new MainExtend();
    thread.start();
    System.out.println("This code is outside of the thread");
  }
  public void run() {
    System.out.println("This code is running in a thread");
  }
}
