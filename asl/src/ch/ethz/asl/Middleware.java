package ch.ethz.asl;

import java.io.*;
import java.net.BindException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
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
  private final List<ServerAddress> servers;

  public Middleware(String myIp, int myPort, List<String> mcAddresses, int numThreadsPTP, int writeToCount) {
    this.port = myPort;
    this.servers = makeServers(mcAddresses);
  }

  private static List<ServerAddress> makeServers(List<String> servers) {
    List<ServerAddress> addrs = new ArrayList<>();
    for (String server : servers) {
      addrs.add(new ServerAddress(server));
    }
    return addrs;
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
      assc.accept(null, new AdhocCompletionHandler(assc, servers));
    } catch (BindException e) {
      e.printStackTrace();
      System.exit(1);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
