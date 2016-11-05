package ch.ethz.asl;

import java.io.IOException;
import java.net.BindException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.channels.AsynchronousChannelGroup;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Middleware {

  private static final Logger log = Logger.getGlobal();

  private final SocketAddress myAddress;
  private final List<ServerAddress> servers;
  public static boolean shuttingDown = false;
  private final AsynchronousChannelGroup group;
  private final AsynchronousServerSocketChannel assc;
  private final int replicationFactor;
  private final int readThreadpoolSize;
  private final Instrumentor instrumentor;

  public static final int BUFFER_SIZE = 1 << 16;

  public Middleware(
      String myIp,
      int myPort,
      List<String> mcAddresses,
      int numThreadsPTP,
      int writeToCount,
      String logfile)
      throws IOException {
    this.myAddress = new InetSocketAddress(myIp, myPort);
    this.servers = makeServers(mcAddresses);
    this.group = AsynchronousChannelGroup.withThreadPool(Executors.newSingleThreadExecutor());
    this.assc = AsynchronousServerSocketChannel.open(group);
    this.replicationFactor = writeToCount;
    this.readThreadpoolSize = numThreadsPTP;
    this.instrumentor = new Instrumentor(logfile);
  }

  private static List<ServerAddress> makeServers(List<String> servers) {
    List<ServerAddress> addrs = new ArrayList<>();
    for (String server : servers) {
      addrs.add(new ServerAddress(server));
    }
    return addrs;
  }

  public static void shutdown() {
    if (shuttingDown) {
      return;
    } // Don't shut down twice.
    shuttingDown = true;
    log.setLevel(Level.INFO);
    log.info("Shutting down entire middleware.");
  }

  public static boolean isShuttingDown() {
    return shuttingDown;
  }

  public void run() {
    startServer();
    sleepForever();
  }

  public void startServer() {
    try {
      assc.bind(myAddress);
      AcceptCompletionHandler ach =
          new AcceptCompletionHandler(
              assc, servers, instrumentor, replicationFactor, readThreadpoolSize);
      assc.accept(null, ach);
    } catch (BindException e) {
      e.printStackTrace();
      System.exit(1);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private void sleepForever() {
    while (true) {
      try {
        if (isShuttingDown()) {
          try {
            assc.close();
            group.shutdown();
          } catch (IOException e) {
            e.printStackTrace();
          }
        }
        Thread.sleep(100);
      } catch (InterruptedException e) {
        e.printStackTrace();
        break;
      }
    }
  }
}
