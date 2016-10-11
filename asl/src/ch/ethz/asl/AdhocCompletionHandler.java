package ch.ethz.asl;

import ch.ethz.asl.request.Request;
import ch.ethz.asl.request.RequestPacket;
import ch.ethz.asl.request.request_parsing.NotEnoughDataException;
import ch.ethz.asl.request.request_parsing.ParseFailedException;
import ch.ethz.asl.request.request_parsing.RequestParser;
import ch.ethz.asl.response.ClientErrorResponse;
import ch.ethz.asl.response.ErrorResponse;
import ch.ethz.asl.response.Response;

import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.logging.Logger;

public class AdhocCompletionHandler
    implements CompletionHandler<AsynchronousSocketChannel, Object> {
  private static final Logger log = Logger.getGlobal();
  private final AsynchronousServerSocketChannel assc;
  private final List<ServerHandler> servers;

  public AdhocCompletionHandler(
      final AsynchronousServerSocketChannel assc, final List<ServerAddress> servers) {
    this.assc = assc;
    this.servers = mkServerHandlers(servers);
  }

  private static List<ServerHandler> mkServerHandlers(final List<ServerAddress> serverAddresses) {
    List<ServerHandler> handlers = new ArrayList();
    for (ServerAddress serverAddress : serverAddresses) {
      handlers.add(new ServerHandler(serverAddress));
    }
    return handlers;
  }

  @Override
  public void completed(final AsynchronousSocketChannel chan, final Object attachment) {
    InitialInput ii = new InitialInput(chan);
    ii.doTheRead(new InitialInputCompletionHandler());
  }

  class InitialInput {
    public ByteBuffer bbuf;
    public final AsynchronousSocketChannel chan;

    private static final int BUFFER_SIZE = 1 << 6;

    public InitialInput(final AsynchronousSocketChannel chan) {
      this.bbuf = ByteBuffer.allocate(BUFFER_SIZE);
      this.chan = chan;
    }

    public void doTheRead(final InitialInputCompletionHandler handler) {
      this.bbuf =
          ByteBuffer.allocate(BUFFER_SIZE); // Clear does not set values to 0, just reallocate here.
      chan.read(bbuf, this, handler);
    }
  }

  class InitialInputCompletionHandler implements CompletionHandler<Integer, InitialInput> {

    @Override
    public void completed(final Integer bytesRead, final InitialInput initialInput) {
      doTheThing(bytesRead, initialInput);
      goOn(initialInput);
    }

    private void doTheThing(int bytesRead, final InitialInput initialInput) {
      AsynchronousSocketChannel chan = initialInput.chan;
      ByteBuffer bbuf = initialInput.bbuf;
      log.finest("Input from client:");
      log.finest(Integer.toString(bytesRead) + " bytes");
      if (bytesRead >= 0) {
        log.finest("\"" + new String(bbuf.array()) + "\"");

        Request req = null;
        Response preliminaryResponse = null;
        try {
          req = RequestParser.parseRequest(bbuf);
          log.finest("Parsed request: " + req.toString());
        } catch (NotEnoughDataException e) {
          log.finest("Not enough data to parse request.");
          preliminaryResponse = new ClientErrorResponse("Not enough data.");
        } catch (ParseFailedException e) {
          log.finest("Failed to parse a request.");
          preliminaryResponse = new ErrorResponse();
        }
        if (req == null) { // Then preliminary response will not be null.
          try {
            RequestPacket.respond(chan, preliminaryResponse);
          } catch (InterruptedException e) {
            e.printStackTrace();
          } catch (ExecutionException e) {
            e.printStackTrace();
          }
        } else {
          RequestPacket packet = new RequestPacket(chan, req);
          ServerHandler sh = pickServer(servers, req);
          try {
            sh.handle(packet);
          } catch (ExecutionException e) {
            e.printStackTrace();
          } catch (InterruptedException e) {
            e.printStackTrace();
          }
        }
      }
    }

    private void goOn(InitialInput initialInput) {
      initialInput.doTheRead(this);
    }

    @Override
    public void failed(Throwable exc, InitialInput attachment) {}
  }

  private static ServerHandler pickServer(final List<ServerHandler> servers, final Request req) {
    int hash = req.hashCode();
    log.finest("hash of request: " + Integer.toString(req.hashCode()));
    int nrServers = servers.size();
    int serverIndex = mod(hash, nrServers);
    log.finest("Chose server index: " + Integer.toString(serverIndex));
    return servers.get(serverIndex);
  }

  private static int mod(final int x, final int y) {
    int result = x % y;
    if (result < 0) {
      result += y;
    }
    return result;
  }

  @Override
  public void failed(Throwable exc, Object attachment) {}
}
