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
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

public class Middleware {

  private static final Logger logger = Logger.getGlobal();

  private final int port;
  private final List<String> servers;

  public Middleware(String myIp, int myPort, List<String> mcAddresses, int numThreadsPTP, int writeToCount) {
    this.port = myPort;
    this.servers = mcAddresses;
  }

  public void run() {
    logger.setLevel(Level.ALL);
    logger.addHandler(new AdhocLogger());
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

          int bufferSize = 1 << 16;

          SocketChannel asc = null;
          try {
            asc = SocketChannel.open();
            asc.configureBlocking(true);
            asc.connect(address);
            while (true) {
              logger.log(Level.FINER, "Spinning");
              ByteBuffer bbuf = ByteBuffer.allocate(bufferSize); // TODO use my own immutable byte buffers.
              int bytesRead = chan.read(bbuf).get();
              logger.finer("Input from client:");
              logger.finer(Integer.toString(bytesRead) + " bytes");
              logger.finest(new String(bbuf.array()));
              int bytesWritten = asc.write(ByteBuffer.wrap(bbuf.array(), 0, bytesRead));
              logger.log(Level.FINER, "Sent " + Integer.toString(bytesWritten) + " to server");

              ByteBuffer bbuf2 = ByteBuffer.allocate(bufferSize);
              int bytesRead2 = asc.read(bbuf2);
              logger.finer("Input from Server:");
              logger.finer(Integer.toString(bytesRead2) + " bytes");
              if (bytesRead2 >= 0) {
                logger.finest(new String(bbuf2.array()));
                int bytesWritten2 = chan.write(ByteBuffer.wrap(bbuf2.array(), 0, bytesRead2)).get();
                logger.finer("Sent " + Integer.toString(bytesWritten2) + " to client");
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
