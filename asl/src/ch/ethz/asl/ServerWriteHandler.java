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
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Logger;

public class ServerWriteHandler {
  private final ServerAddress serverAddress;
  private static final Logger log = Logger.getGlobal();
  private final ExecutorService executor;
  private SocketChannel serverConnection;

  public ServerWriteHandler(final ServerAddress serverAddress) {
    this.serverAddress = serverAddress;
    executor = Executors.newSingleThreadExecutor();
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

  public void handle(final RequestPacket req) throws ExecutionException, InterruptedException {
    executor.submit(new WriteTask(req));
  }

  class WriteTask implements Runnable {
    private final RequestPacket packet;

    public WriteTask(final RequestPacket packet) {
      this.packet = packet;
    }

    @Override
    public void run() {
      Response resp = handleToGetResponse(packet.getRequest());
      try {
        packet.respond(resp);
      } catch (InterruptedException | ExecutionException e) {
        e.printStackTrace(); // FIXME handle this somehow
      }
    }
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
