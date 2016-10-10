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
import java.util.logging.Logger;

public class ServerHandler {
  private static final Logger log = Logger.getGlobal();
  private final ServerAddress serverAddress;

  public ServerHandler(final ServerAddress serverAddress) {
    this.serverAddress = serverAddress;
  }

  public void handle(final RequestPacket req) throws ExecutionException, InterruptedException {
    Response resp = handlePure(req.getRequest());
    req.respond(resp);
  }

  public Response handlePure(final Request req) {
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
