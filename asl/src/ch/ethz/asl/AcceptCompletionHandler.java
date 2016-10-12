package ch.ethz.asl;

import ch.ethz.asl.request.Request;
import ch.ethz.asl.request.RequestPacket;
import ch.ethz.asl.request.request_parsing.NotEnoughDataException;
import ch.ethz.asl.request.request_parsing.ParseFailedException;
import ch.ethz.asl.request.request_parsing.RequestParser;
import ch.ethz.asl.response.ClientErrorResponse;
import ch.ethz.asl.response.ErrorResponse;
import ch.ethz.asl.response.Response;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.logging.Logger;

import static ch.ethz.asl.Middleware.BUFFER_SIZE;
import static ch.ethz.asl.Middleware.shutdown;

public class AcceptCompletionHandler
    implements CompletionHandler<AsynchronousSocketChannel, Object> {
  private static final Logger log = Logger.getGlobal();
  private final AsynchronousServerSocketChannel assc;
  private final List<ServerHandler> servers;

  public AcceptCompletionHandler(
      final AsynchronousServerSocketChannel assc, final List<ServerAddress> servers) {
    this.assc = assc;
    this.servers = mkServerHandlers(servers);
  }

  private static List<ServerHandler> mkServerHandlers(final List<ServerAddress> serverAddresses) {
    List<ServerHandler> handlers = new ArrayList<>();
    for (ServerAddress serverAddress : serverAddresses) {
      handlers.add(new ServerHandler(serverAddress));
    }
    return handlers;
  }

  @Override
  public void completed(final AsynchronousSocketChannel chan, final Object attachment) {
    assc.accept(null, this); // Ready to accept the next one.
    InitialInput ii = new InitialInput(chan);
    ii.doTheRead(new InitialInputCompletionHandler());
  }

  class InitialInput {
    ByteBuffer bbuf;
    final AsynchronousSocketChannel chan;

    InitialInput(final AsynchronousSocketChannel chan) {
      this.bbuf = ByteBuffer.allocate(BUFFER_SIZE);
      this.chan = chan;
    }

    void doTheRead(final InitialInputCompletionHandler handler) {
      this.bbuf =
          ByteBuffer.allocate(BUFFER_SIZE); // Clear does not set values to 0, just reallocate here.
      chan.read(bbuf, this, handler);
    }
  }

  class InitialInputCompletionHandler implements CompletionHandler<Integer, InitialInput> {

    @Override
    public void completed(final Integer bytesRead, final InitialInput initialInput) {
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
          } catch (InterruptedException | ExecutionException e) {
            e.printStackTrace();
          }
        } else {
          RequestPacket packet = new RequestPacket(chan, req);
          ServerHandler sh = pickServer(servers, req);
          try {
            sh.handle(packet);
          } catch (ExecutionException | InterruptedException e) {
            e.printStackTrace();
          }
        }
        // Go on reading
        initialInput.doTheRead(this);
      } else {
        try {
          chan.close();
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    }

    @Override
    public void failed(Throwable exc, InitialInput initialInput) {
      try {
        initialInput.chan.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
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
  public void failed(Throwable exc, Object attachment) {
    try {
      assc.close();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
