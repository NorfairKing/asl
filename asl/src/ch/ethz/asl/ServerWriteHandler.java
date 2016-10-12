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

public class ServerWriteHandler {
  private final ServerHandler serverHandler;
  private final ServerAddress serverAddress;
  private static final Logger log = Logger.getGlobal();
  private final ExecutorService executor;
  private final BlockingQueue<RequestPacket> writequeue;

  public ServerWriteHandler(ServerHandler serverHandler, final ServerAddress serverAddress) {
    this.serverHandler = serverHandler;
    this.serverAddress = serverAddress;
    writequeue = new LinkedBlockingQueue<>();
    executor = Executors.newSingleThreadExecutor();
    executor.submit(new WriteWorker());
  }

  public void handle(final RequestPacket req) throws InterruptedException {
    writequeue.put(req);
  }

  private void shutdown() {
    serverHandler.shutdown();
  }

  class WriteWorker implements Runnable {
    private SocketChannel serverConnection;

    public WriteWorker() {
      connect();
    }

    private void connect() {
      SocketAddress address = serverAddress.getSocketAddress();
      log.finest("Writer connecting to: " + address);
      serverConnection = null;
      try {
        serverConnection = SocketChannel.open();
        serverConnection.configureBlocking(true);
        serverConnection.connect(address);
        log.finest("Writer connected to: " + address);
      } catch (IOException e) {
        e.printStackTrace();
      }
    }

    @Override
    public void run() {
      RequestPacket packet = null;
      try {
        packet = writequeue.take();
      } catch (InterruptedException e) {
        log.fine(
            "Write worker for server: "
                + serverAddress.getSocketAddress()
                + " was interrupted while waiting for requests/");
        return;
      }
      Response resp = handleToGetResponse(packet.getRequest());
      try {
        packet.respond(resp);
      } catch (InterruptedException | ExecutionException e) {
        e.printStackTrace(); // FIXME handle this somehow
      }
      if (ServerWriteHandler.this.serverHandler.isShuttingDown()) {
        log.fine("Shutting down write worker for server: " + serverAddress.getSocketAddress());
        return;
      }
      executor.submit(this); // Then we don't have to while(true)
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
