package ch.ethz.asl;

import java.io.*;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.nio.channels.SocketChannel;
import java.util.List;
import java.util.concurrent.ExecutionException;

public class Middleware {

  private final int port;
  private final List<String> servers;

  public Middleware(String myIp, int myPort, List<String> mcAddresses, int numThreadsPTP, int writeToCount) {
    this.port = myPort;
    this.servers = mcAddresses;
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

  public void startServer() {
    try {
      AsynchronousServerSocketChannel assc = AsynchronousServerSocketChannel.open().bind(new InetSocketAddress(port));
      assc.accept(null, new CompletionHandler<AsynchronousSocketChannel, Object>() {
        @Override
        public void completed(AsynchronousSocketChannel chan, Object attachment) {
          assc.accept(attachment, this);

          String server = servers.get(0);
          String serverUrl = server.split(":")[0];
          int serverPort = Integer.parseInt(server.split(":")[1]);

          SocketAddress address = new InetSocketAddress(serverUrl, serverPort);
          System.out.println("Connecting to: " + address);
          SocketChannel asc = null;
          try {
            asc = SocketChannel.open();
            asc.configureBlocking(true);
            asc.connect(address);
            while (true) {
              System.out.println("Spinning");
              ByteBuffer bbuf = ByteBuffer.allocate(4096); // TODO use my own immutable byte buffers.
              int bytesRead = chan.read(bbuf).get();
              System.out.println("Input from client:");
              System.out.println(Integer.toString(bytesRead) + " bytes");
              System.out.println(new String(bbuf.array()));
              int bytesWritten = asc.write(bbuf);
              System.out.print("Sent " + Integer.toString(bytesWritten) + " to server");

              ByteBuffer bbuf2 = ByteBuffer.allocate(4096);
              int bytesRead2 = asc.read(bbuf2);
              System.out.println("Input from Server:");
              System.out.println(Integer.toString(bytesRead2) + " bytes");
              if (bytesRead2 >= 0) {
                System.out.println(new String(bbuf2.array()));
                int bytesWritten2 = chan.write(bbuf2).get();
                System.out.print("Sent " + Integer.toString(bytesWritten) + " to client");
              }
            }
          } catch (IOException | InterruptedException | ExecutionException e) {
            e.printStackTrace();
          } finally {
            try {
              if (asc != null) {
                asc.close();
              }
              if (chan != null) {
                chan.close();
              }
            } catch (IOException e) {
              e.printStackTrace();
            }
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
