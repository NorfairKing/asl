package ch.ethz.asl.request;

import ch.ethz.asl.Instrumentor;
import ch.ethz.asl.response.Response;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.util.concurrent.ExecutionException;
import java.util.logging.Logger;

public class RequestPacket {
  private static final Logger log = Logger.getGlobal();
  private final AsynchronousSocketChannel chan;
  private final Request req;

  private final Instrumentor instrumentor;

  private long receivedTime;
  private long respondedTime;

  public RequestPacket(
      final AsynchronousSocketChannel chan, final Request req, final Instrumentor instrumentor) {
    this.chan = chan;
    this.req = req;
    this.instrumentor = instrumentor;
  }

  public void respond(Response res) throws InterruptedException, ExecutionException, IOException {
    RequestPacket.respond(this.chan, res);
    instrumentor.finaliseRequest(this);
  }

  public static void respond(final AsynchronousSocketChannel chan, final Response res)
      throws InterruptedException, ExecutionException {
    log.finest("Produced response: " + res.toString());

    ByteBuffer responseBytes = res.render();
    responseBytes.position(0);
    int bytesWritten2 = chan.write(responseBytes).get();
    log.finest("Sent " + Integer.toString(bytesWritten2) + " to client");
    if (bytesWritten2 <= 0) {
      log.fine("Failed to write response to client, closing connection.");
      try {
        chan.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  public long getReceivedAt() {
    return receivedTime;
  }

  public void setReceivedAt(long time) {
    receivedTime = time;
  }

  public long getRespondedAt() {
    return receivedTime;
  }

  private void setResponded() {
    receivedTime = System.currentTimeMillis();
  }

  public Request getRequest() {
    return req;
  }
}
