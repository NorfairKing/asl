package ch.ethz.asl;

import ch.ethz.asl.request.Request;
import ch.ethz.asl.request.RequestPacket;
import ch.ethz.asl.response.Response;
import ch.ethz.asl.response.ServerErrorResponse;
import ch.ethz.asl.response.SuccessfulResponse;

import java.io.IOException;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.concurrent.*;
import java.util.logging.Logger;

import static ch.ethz.asl.Middleware.BUFFER_SIZE;
import static ch.ethz.asl.Middleware.shutdown;

public class ServerReadHandler {
  private final ServerHandler serverHandler;
  private final ServerAddress serverAddress;
  private static final Logger log = Logger.getGlobal();
  private final ExecutorService threadPool;
  private final BlockingQueue<RequestPacket> readqueue;

  public ServerReadHandler(final ServerHandler serverHandler, final ServerAddress serverAddress) {
    this.serverHandler = serverHandler;
    this.serverAddress = serverAddress;
    int nrThreads = 1; // fixme take this from the flags.
    this.readqueue = new LinkedBlockingQueue<>();
    this.threadPool = Executors.newFixedThreadPool(nrThreads);
    for (int i = 0; i < nrThreads; i++) {
      threadPool.submit(new ReadWorker(i));
    }
  }

  public void handle(final RequestPacket req) throws InterruptedException {
    readqueue.put(req);
  }

  class ReadWorker implements Runnable {
    private SocketChannel serverConnection;
    private final int readWorkerIndex;

    public ReadWorker(int index) {
      this.readWorkerIndex = index;
      connect();
    }

    private void connect() {
      SocketAddress address = serverAddress.getSocketAddress();
      log.finest("Read connecting to: " + address);
      serverConnection = null;
      try {
        serverConnection = SocketChannel.open();
        serverConnection.configureBlocking(true);
        serverConnection.connect(address);
        log.finest("Reader connected to: " + address);
      } catch (IOException e) {
        e.printStackTrace();
        shutdown();
      }
    }

    @Override
    public void run() {
      RequestPacket packet = null;
      try {
        packet = readqueue.take();
      } catch (InterruptedException e) {
        e.printStackTrace();
        return;
      }
      Response resp = handleToGetResponse(packet.getRequest());
      try {
        packet.respond(resp);
      } catch (InterruptedException | ExecutionException | IOException e) {
        e.printStackTrace(); // FIXME handle this somehow
      }
      if (ServerReadHandler.this.serverHandler.isShuttingDown()) {
        log.fine(
            "Shutting down read worker "
                + readWorkerIndex
                + " for server "
                + serverAddress.getSocketAddress());
        return;
      }

      threadPool.submit(this); // Then we don't have to while(true)
    }

    public Response handleToGetResponse(final Request req) {
      ByteBuffer rbuf = req.render();
      rbuf.position(0);
      int bytesWritten;
      try {
        bytesWritten = serverConnection.write(rbuf);
      } catch (IOException e) {
        shutdown();
        return new ServerErrorResponse(
            "Failed to write to server: " + serverAddress.getSocketAddress());
      }
      if (bytesWritten <= 0) {
        shutdown();
        return new ServerErrorResponse("Wrote " + bytesWritten + " bytes to server.");
      }
      log.finest("Sent " + bytesWritten + " to server:");
      log.finest(new String(rbuf.array()));

      ByteBuffer bbuf2 = ByteBuffer.allocate(BUFFER_SIZE);
      int bytesRead2;
      try {
        bytesRead2 = serverConnection.read(bbuf2);
      } catch (IOException e) {
        shutdown();
        return new ServerErrorResponse(
            "Failed to receive from server: " + serverAddress.getSocketAddress());
      }
      if (bytesRead2 <= 0) {
        shutdown();
        return new ServerErrorResponse(
            bytesRead2 + " bytes read from server: " + serverAddress.getSocketAddress());
      }
      log.finest("Input from Server:");
      log.finest(bytesRead2 + " bytes");
      return new SuccessfulResponse(ByteBuffer.wrap(bbuf2.array(), 0, bytesRead2));
    }
  }
}
