package ch.ethz.asl;

import ch.ethz.asl.generic_parsing.NotEnoughDataException;
import ch.ethz.asl.generic_parsing.ParseFailedException;
import ch.ethz.asl.request.Request;
import ch.ethz.asl.request.RequestPacket;
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
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.logging.Logger;

import static ch.ethz.asl.Instrumentor.now;
import static ch.ethz.asl.Middleware.BUFFER_SIZE;

public class AcceptCompletionHandler
    implements CompletionHandler<AsynchronousSocketChannel, Object> {
  private static final Logger log = Logger.getGlobal();
  private final AsynchronousServerSocketChannel assc;
  private final Instrumentor instrumentor;
  private final List<ServerHandler> servers;
  private final int replicationFactor;

  public AcceptCompletionHandler(
      final AsynchronousServerSocketChannel assc,
      final List<ServerAddress> servers,
      final Instrumentor instrumentor,
      final int replicationFactor,
      final int readThreadpoolSize) {
    this.assc = assc;
    this.servers = mkServerHandlers(servers, readThreadpoolSize);
    this.instrumentor = instrumentor;
    this.replicationFactor = replicationFactor;
  }

  private static List<ServerHandler> mkServerHandlers(
      final List<ServerAddress> serverAddresses, final int readThreadpoolSize) {
    List<ServerHandler> handlers = new ArrayList<>();
    for (ServerAddress serverAddress : serverAddresses) {
      handlers.add(new ServerHandler(serverAddress, readThreadpoolSize));
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
      long receivedAt = now();
      AsynchronousSocketChannel chan = initialInput.chan;
      ByteBuffer bbuf = initialInput.bbuf;
      // log.finest("Input from client:");
      //      log.finest(Integer.toString(bytesRead) + " bytes");
      if (bytesRead >= 0) {
        //        log.finest("\"" + new String(bbuf.array()) + "\"");

        Request req = null;
        Response preliminaryResponse = null;
        try {
          req = RequestParser.parseRequest(bbuf);
          //          log.finest("Parsed request: " + req.toString());
        } catch (NotEnoughDataException e) {
          log.fine("Not enough data to parse request.");
          preliminaryResponse = new ClientErrorResponse("Not enough data.");
        } catch (ParseFailedException e) {
          log.fine("Failed to parse a request.");
          preliminaryResponse = new ErrorResponse();
        }
        if (req == null) { // Then preliminary response will not be null.
          try {
            RequestPacket.respond(chan, preliminaryResponse);
          } catch (InterruptedException | ExecutionException e) {
            e.printStackTrace();
          }
        } else {
          RequestPacket packet = new RequestPacket(chan, req, instrumentor);
          packet.setReceivedAt(receivedAt);
          packet.setParsed();
          sendToServerHandlers(packet);
        }
        // Go on reading
        initialInput.doTheRead(this);
      } else {
        try {
          log.fine("Got " + bytesRead + " from client, closing the connection.");
          chan.close();
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    }

    @Override
    public void failed(Throwable exc, InitialInput initialInput) {
      try {
        log.info("Initial input failed, closing the connection.");
        initialInput.chan.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  private void sendToServerHandlers(RequestPacket packet) {
    List<ServerHandler> handlers = new LinkedList<>();
    switch (packet.getRequest().getKind()) {
      case READ_REQUEST:
        handlers.add(servers.get(pickServerIndex(servers, packet.getRequest())));
        break;
      case WRITE_REQUEST:
        int initix = pickServerIndex(servers, packet.getRequest());
        for (int i = 0; i < replicationFactor; i++) {
          handlers.add(servers.get((initix + i) % servers.size()));
        }
        break;
    }
    packet.setReplicationCounter(handlers.size());
    try {
      for (ServerHandler sh : handlers) {
        sh.handle(packet);
      }
    } catch (ExecutionException | InterruptedException e) {
      e.printStackTrace();
    }
  }

  private static int pickServerIndex(final List<ServerHandler> servers, final Request req) {
    int hash = req.keyHash();
    //    log.finest("hash of request: " + Integer.toString(req.hashCode()));
    int nrServers = servers.size();
    int serverIndex = mod(hash, nrServers);
    //    log.finest("Chose server index: " + Integer.toString(serverIndex));
    return serverIndex;
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
