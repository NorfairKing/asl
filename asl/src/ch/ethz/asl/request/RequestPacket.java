package ch.ethz.asl.request;

import ch.ethz.asl.response.Response;

import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.util.concurrent.ExecutionException;
import java.util.logging.Logger;

public class RequestPacket {
  private static final Logger log = Logger.getGlobal();
  private final AsynchronousSocketChannel chan;
  private final Request req;

  public RequestPacket(final AsynchronousSocketChannel chan, final Request req) {
    this.chan = chan;
    this.req = req;
  }

  public void respond(Response res) throws InterruptedException, ExecutionException {
    RequestPacket.respond(this.chan, res);
  }

  public static void respond(final AsynchronousSocketChannel chan, final Response res)
      throws InterruptedException, ExecutionException {
    log.finest("Produced response: " + res.toString());

    ByteBuffer responseBytes = res.render();
    responseBytes.position(0);
    int bytesWritten2 = chan.write(responseBytes).get();
    log.finest("Sent " + Integer.toString(bytesWritten2) + " to client");
  }

  public Request getRequest() {
    return req;
  }
}
