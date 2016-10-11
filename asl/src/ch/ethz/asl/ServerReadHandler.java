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

public class ServerReadHandler {
  private final ServerAddress serverAddress;
  private static final Logger log = Logger.getGlobal();
  private final ExecutorService threadPool;
  private final BlockingQueue<RequestPacket> readqueue;

  public ServerReadHandler(final ServerAddress serverAddress) {
    this.serverAddress = serverAddress;
    int nrThreads = 1; // fixme take this from the flags.
    this.readqueue = new LinkedBlockingQueue<>();
    this.threadPool = Executors.newFixedThreadPool(nrThreads);
    for (int i = 0; i < nrThreads; i++) {
      threadPool.submit(new ReadWorker());
    }
  }

  public void handle(final RequestPacket req) throws ExecutionException, InterruptedException {
    readqueue.put(req); // Put is the blocking version. We may have to limit the size of the queue.
  }

  class ReadWorker implements Runnable {
    private SocketChannel serverConnection;

    public ReadWorker() {
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
      } catch (InterruptedException e) {
        e.printStackTrace(); // FIXME handle this somehow
      } catch (ExecutionException e) {
        e.printStackTrace(); // FIXME handle this somehow
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
        return new ServerErrorResponse(
            "Failed to write to server: " + serverAddress.getSocketAddress());
      }
      log.finest("Sent " + Integer.toString(bytesWritten) + " to server:");
      log.finest(new String(rbuf.array()));

      int bufferSize = 1 << 16;
      ByteBuffer bbuf2 = ByteBuffer.allocate(bufferSize);
      int bytesRead2;
      try {
        bytesRead2 = serverConnection.read(bbuf2);
      } catch (IOException e) {
        return new ServerErrorResponse(
            "Failed to receive from server: " + serverAddress.getSocketAddress());
      }
      log.finest("Input from Server:");
      log.finest(Integer.toString(bytesRead2) + " bytes");
      if (bytesRead2 > 0) {
        return new SuccessfulResponse(ByteBuffer.wrap(bbuf2.array(), 0, bytesRead2));
      } else {
        return new ServerErrorResponse(
            "0 bytes read from server: " + serverAddress.getSocketAddress());
      }
    }
  }
}
