package ch.ethz.asl;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.util.List;
import java.util.concurrent.ExecutionException;

public class Middleware {

  private static final int PORT = 11212;

  public Middleware(String myIp, int myPort, List<String> mcAddresses, int numThreadsPTP, int writeToCount) {
  }

  public void run() {
    startServer();
    sleepForever();
  }

  private static void sleepForever() {
    while (true) {
      try {
        Thread.sleep(Long.MAX_VALUE);
      } catch (InterruptedException e) {
        break;
      }
    }
  }

  public static void startServer() {
    try {
      AsynchronousServerSocketChannel assc = AsynchronousServerSocketChannel.open().bind(new InetSocketAddress(PORT));
      assc.accept(null, new CompletionHandler<AsynchronousSocketChannel, Object>() {
        @Override
        public void completed(AsynchronousSocketChannel chan, Object attachment) {
          System.out.println("working");
          assc.accept(attachment, this);
          ByteBuffer bbuf = ByteBuffer.allocate(4096); // TODO use my own immutable byte buffers.

          try {
            chan.close();
          } catch (IOException e) {
            e.printStackTrace();
          }
        }

        @Override
        public void failed(Throwable exc, Object attachment) {
        }
      });
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
