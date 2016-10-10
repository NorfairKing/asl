package ch.ethz.asl;

import ch.ethz.asl.request.Request;
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

      Request req;
      Response res;
      try {
        req = RequestParser.parseRequest(bbuf);
        log.finest("Parsed request: " + req.toString());
        res = handle(req);
      } catch (NotEnoughDataException e) {
        res = new ClientErrorResponse("Not enough data.");
      } catch (ParseFailedException e) {
        res = new ErrorResponse();
      }
      log.finest("Produced response: " + res.toString());

      ByteBuffer responseBytes = res.render();
      responseBytes.position(0);
      int bytesWritten2 = chan.write(responseBytes).get();
      log.finest("Sent " + Integer.toString(bytesWritten2) + " to client");
    }
  }

  private Response handle(final Request req) {
    ServerHandler sh = pickServer(servers, req);
    return sh.handle(req);
  }

  private static ServerHandler pickServer(final List<ServerHandler> servers, final Request req) {
    int hash = req.hashCode();
    int nrServers = servers.size();
    int serverIndex = mod(hash, nrServers);
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
