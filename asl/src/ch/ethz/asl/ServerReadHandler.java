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

  public ServerReadHandler(
      final ServerHandler serverHandler,
      final ServerAddress serverAddress,
      final int readThreadpoolSize) {
    this.serverHandler = serverHandler;
    this.serverAddress = serverAddress;
    this.readqueue = new LinkedBlockingQueue<>();
    this.threadPool =
        Executors.newFixedThreadPool(readThreadpoolSize); // Leave room to submit itself.
    for (int i = 0; i < readThreadpoolSize; i++) {
      threadPool.submit(new ReadWorker(i));
    }
  }

  public void handle(final RequestPacket req) throws InterruptedException {
    log.fine("Putting on read queue for server" + serverAddress + " now sized " + readqueue.size());
    readqueue.put(req);
    log.fine("Put on read queue for server " + serverAddress + " now sized " + readqueue.size());
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
      log.fine("Read connecting to: " + address);
      serverConnection = null;
      try {
        serverConnection = SocketChannel.open();
        serverConnection.configureBlocking(true);
        serverConnection.connect(address);
        log.fine("Reader connected to: " + address);
      } catch (IOException e) {
        e.printStackTrace();
        shutdown();
      }
    }

    @Override
    public void run() {
      while (true) {
        if (ServerReadHandler.this.serverHandler.isShuttingDown()) {
          log.fine(
              "Shutting down read worker "
                  + readWorkerIndex
                  + " for server "
                  + serverAddress.getSocketAddress());
          return; // Stop.
        }

        try {
          handleOneRequest();
        } catch (InterruptedException e) {
          e.printStackTrace();
        }
      }
    }

    private void handleOneRequest() throws InterruptedException {
      RequestPacket packet;
      log.fine(
          readWorkerIndex
              + " dequeuing from readqueue for server "
              + serverAddress
              + " now sized "
              + readqueue.size());
      packet = readqueue.take();
      log.fine(
          readWorkerIndex
              + " dequeud from readqueue for server "
              + serverAddress
              + " now sized "
              + readqueue.size());
      packet.setDequeued();
      log.fine("Handling to get response.");
      Response resp = handleToGetResponse(packet);
      log.fine("Got read response.");
      try {
        packet.respond(resp);
      } catch (ExecutionException | IOException e) {
        e.printStackTrace(); // FIXME handle this somehow
      }
      log.fine(
          "Readworker " + readWorkerIndex + " for server " + serverAddress + " done with request.");
    }

    private Response handleToGetResponse(final RequestPacket packet) {
      Request req = packet.getRequest();
      ByteBuffer rbuf = req.render();
      rbuf.position(0);
      int bytesWritten;
      try {
        bytesWritten = serverConnection.write(rbuf);
      } catch (IOException e) {
        e.printStackTrace();
        shutdown();
        return new ServerErrorResponse(
            "Failed to write to server: " + serverAddress.getSocketAddress());
      }
      log.fine("Wrote " + bytesWritten + " bytes to server.");
      if (bytesWritten <= 0) {
        shutdown();
        return new ServerErrorResponse("Wrote " + bytesWritten + " bytes to server.");
      }
      log.finest("Sent " + bytesWritten + " to server:");
      log.finest(new String(rbuf.array()));
      packet.setAsked();

      ByteBuffer bbuf2 = ByteBuffer.allocate(BUFFER_SIZE);
      int bytesRead2;
      try {
        bytesRead2 = serverConnection.read(bbuf2);
      } catch (IOException e) {
        e.printStackTrace();
        shutdown();
        return new ServerErrorResponse(
            "Failed to receive from server: " + serverAddress.getSocketAddress());
      }
      log.fine("Read " + bytesRead2 + " bytes from server.");
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
