package eu.cssyd.asl;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;

public class AslMain {
  public static void main(String[] args) {
    System.out.println("ASL");
    new Middleware().startServer();
    sleepForever();
  }

  private static void sleepForever() {
    while(true) {
      try {
        Thread.sleep(Long.MAX_VALUE);
      } catch (InterruptedException e) {
        break;
      }
    }
  }
}
