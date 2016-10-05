package ch.ethz.asl;

import ch.ethz.asl.request.Request;
import ch.ethz.asl.request.request_parsing.NotEnoughDataException;
import ch.ethz.asl.request.request_parsing.ParseFailedException;
import ch.ethz.asl.request.request_parsing.RequestParser;
import ch.ethz.asl.response.Response;
import ch.ethz.asl.response.ServerErrorResponse;
import ch.ethz.asl.response.SuccessfulResponse;

import java.io.IOException;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.nio.channels.SocketChannel;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.logging.Logger;

public class AdhocCompletionHandler implements CompletionHandler<AsynchronousSocketChannel, Object> {
  private static final Logger log = Logger.getGlobal();
  private final AsynchronousServerSocketChannel assc;
  private final List<ServerAddress> servers;

  public AdhocCompletionHandler(AsynchronousServerSocketChannel assc, List<ServerAddress> servers) {
    this.assc = assc;
    this.servers = servers;
  }

  @Override
  public void completed(AsynchronousSocketChannel chan, Object attachment) {
    assc.accept(attachment, this);

    int bufferSize = 1 << 16;

    try {
      while (true) {
        log.finest("Spinning");
        ByteBuffer bbuf = ByteBuffer.allocate(bufferSize);
        int bytesRead = chan.read(bbuf).get();
        log.finest("Input from client:");
        log.finest(Integer.toString(bytesRead) + " bytes");
        if (bytesRead >= 0) {
          log.finest(new String(bbuf.array()));

          Request req;
          try {
            req = RequestParser.parseRequest(bbuf);
          } catch (NotEnoughDataException | ParseFailedException e) {
            e.printStackTrace();
            chan.write(ByteBuffer.wrap("ERROR\r\n".getBytes()));
            continue;
          }
          log.finest("Parsed request: " + req.toString());
          Response res = handle(req);

          int bytesWritten2 = chan.write(res.render()).get();
          log.finest("Sent " + Integer.toString(bytesWritten2) + " to client");
        }

      }
    } catch (ExecutionException e) {
      e.printStackTrace();
    } catch (InterruptedException e) {
      e.printStackTrace();
      System.exit(1);
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

  public Response handle(Request req) {

    SocketAddress address = servers.get(0).getSocketAddress();
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

  @Override
  public void failed(Throwable exc, Object attachment) {

  }
}
