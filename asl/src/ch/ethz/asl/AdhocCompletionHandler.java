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
    assc.accept(attachment, this);

    try {
      while (true) {
        spin(chan);
      }
    } catch (ExecutionException e) {
      return; // Just close the connection.
    } catch (InterruptedException e) {
      System.exit(1); // Just stop.
    } finally {
      try {
        if (chan != null) {
          chan.close();
        }
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  private void spin(final AsynchronousSocketChannel chan)
      throws ExecutionException, InterruptedException {
    log.finest("Spinning");
    int bufferSize = 1 << 16;
    ByteBuffer bbuf = ByteBuffer.allocate(bufferSize);
    int bytesRead = chan.read(bbuf).get();
    log.finest("Input from client:");
    log.finest(Integer.toString(bytesRead) + " bytes");
    if (bytesRead >= 0) {
      log.finest(new String(bbuf.array()));

      Request req = null;
      Response preliminaryResponse = null;
      try {
        req = RequestParser.parseRequest(bbuf);
        log.finest("Parsed request: " + req.toString());
      } catch (NotEnoughDataException e) {
        log.finest("Note enough data to parse request.");
        preliminaryResponse = new ClientErrorResponse("Not enough data.");
      } catch (ParseFailedException e) {
        log.finest("Failed to parse a request.");
        preliminaryResponse = new ErrorResponse();
      }
      if (req == null) { // Then preliminary response will not be null.
        RequestPacket.respond(chan, preliminaryResponse);
      } else {
        RequestPacket packet = new RequestPacket(chan, req);
        ServerHandler sh = pickServer(servers, req);
        sh.handle(packet);
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
  }
}
