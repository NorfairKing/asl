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

  public ServerWriteHandler(final ServerAddress serverAddress) {
    this.serverAddress = serverAddress;
    executor = Executors.newSingleThreadExecutor();
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
      } catch (InterruptedException e) {
        e.printStackTrace(); // FIXME handle this somehow
      } catch (ExecutionException e) {
        e.printStackTrace(); // FIXME handle this somehow
      }
    }
  }

  public Response handleToGetResponse(final Request req) {
    SocketAddress address = serverAddress.getSocketAddress();
    log.finest("Connecting to: " + address);
    SocketChannel asc = null;
    try {
      try {
        asc = SocketChannel.open();
        asc.configureBlocking(true);
        asc.connect(address);
      } catch (IOException e) {
        return new ServerErrorResponse("Failed to connect to server.");
      }

      ByteBuffer rbuf = req.render();
      rbuf.position(0);
      int bytesWritten;
      try {
        bytesWritten = asc.write(rbuf);
      } catch (IOException e) {
        return new ServerErrorResponse("Failed to write to server.");
      }
      log.finest("Sent " + Integer.toString(bytesWritten) + " to server:");
      log.finest(new String(rbuf.array()));

      int bufferSize = 1 << 16;
      ByteBuffer bbuf2 = ByteBuffer.allocate(bufferSize);
      int bytesRead2;
      try {
        bytesRead2 = asc.read(bbuf2);
      } catch (IOException e) {
        return new ServerErrorResponse("Failed to receive from server.");
      }
      log.finest("Input from Server:");
      log.finest(Integer.toString(bytesRead2) + " bytes");
      if (bytesRead2 > 0) {
        return new SuccessfulResponse(ByteBuffer.wrap(bbuf2.array(), 0, bytesRead2));
      } else {
        return new ServerErrorResponse("0 bytes read from server.");
      }

    } finally {
      try {
        if (asc != null) {
          asc.close();
        }
      } catch (IOException e) {
        return new ServerErrorResponse("Failed to close socket channel.");
      }
    }
  }
}
