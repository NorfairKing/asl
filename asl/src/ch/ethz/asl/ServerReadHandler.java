package ch.ethz.asl;

import ch.ethz.asl.generic_parsing.NotEnoughDataException;
import ch.ethz.asl.generic_parsing.ParseFailedException;
import ch.ethz.asl.request.Request;
import ch.ethz.asl.request.RequestPacket;
import ch.ethz.asl.response.*;
import ch.ethz.asl.response.response_parsing.ResponseParser;

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
    readqueue.put(req);
    req.setEnqueued();
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
      serverConnection = null;
      try {
        serverConnection = SocketChannel.open();
        serverConnection.configureBlocking(true);
        serverConnection.connect(address);
      } catch (IOException e) {
        e.printStackTrace();
        shutdown();
      }
    }

    @Override
    public void run() {
      Thread.currentThread()
          .setName(
              "ReadWorker " + readWorkerIndex + " for read requests to server " + serverAddress);
      while (true) {
        if (ServerReadHandler.this.serverHandler.isShuttingDown()) {
          log.info(
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
      packet = readqueue.take();
      packet.setDequeued();
      Response resp = handleToGetResponse(packet);
      try {
        packet.respond(resp);
      } catch (ExecutionException | IOException e) {
        e.printStackTrace(); // FIXME handle this somehow
      }
    }

    private Response handleToGetResponse(final RequestPacket packet) {
      Response preliminaryResponse = sendToServer(packet);
      if (preliminaryResponse != null) {
        return preliminaryResponse;
      }
      return getReplyFromServer(packet);
    }

    private Response sendToServer(final RequestPacket packet) {
      Request req = packet.getRequest();
      ByteBuffer rbuf = req.render();
      rbuf.position(0);
      int bytesWritten;
      try {
        bytesWritten = serverConnection.write(rbuf);
      } catch (IOException e) {
        e.printStackTrace();
        shutdown();
        return new ServerErrorResponse("Failed to write to server: " + serverAddress);
      }
      if (bytesWritten <= 0) {
        shutdown();
        return new ServerErrorResponse("Wrote " + bytesWritten + " bytes to server.");
      }
      packet.setAsked();
      return null;
    }

    private Response getReplyFromServer(RequestPacket packet) {
      ByteBuffer bbuf2 = ByteBuffer.allocate(BUFFER_SIZE);
      int bytesRead2;
      try {
        bytesRead2 = serverConnection.read(bbuf2);
      } catch (IOException e) {
        e.printStackTrace();
        shutdown();
        return new ServerErrorResponse("Failed to receive from server: " + serverAddress);
      }
      if (bytesRead2 <= 0) {
        shutdown();
        return new ServerErrorResponse(bytesRead2 + " bytes read from server " + serverAddress);
      }
      Response resp;
      try {
        resp = ResponseParser.parseResponse(bbuf2);
      } catch (NotEnoughDataException e) { // TODO handle this better.
        resp = new ClientErrorResponse("Not enough data.");
        packet.setFailed();
      } catch (ParseFailedException e) {
        resp = new ErrorResponse();
        packet.setFailed();
      }
      return resp;
    }
  }
}
