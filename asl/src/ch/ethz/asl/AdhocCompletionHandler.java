package ch.ethz.asl;

import ch.ethz.asl.request.Request;

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
  private static final Logger logger = Logger.getGlobal();
  private final AsynchronousServerSocketChannel assc;
  private final List<ServerAddress> servers;

  public AdhocCompletionHandler(AsynchronousServerSocketChannel assc, List<ServerAddress> servers) {
    this.assc = assc;
    this.servers = servers;
  }

  @Override
  public void completed(AsynchronousSocketChannel chan, Object attachment) {
    assc.accept(attachment, this);

    SocketAddress address = servers.get(0).getSocketAddress();
    System.out.println("Connecting to: " + address);

    int bufferSize = 1 << 16;

    SocketChannel asc = null;
    try {
      asc = SocketChannel.open();
      asc.configureBlocking(true);
      asc.connect(address);
      while (true) {
        logger.finer("Spinning");
        ByteBuffer bbuf = ByteBuffer.allocate(bufferSize);
        int bytesRead = chan.read(bbuf).get();
        logger.finest("Input from client:");
        logger.finest(Integer.toString(bytesRead) + " bytes");
        if (bytesRead >= 0) {
          logger.finer(new String(bbuf.array()));

          Request req = null;
          try {
            req = Request.parseRequest(bbuf);
          } catch (Request.NotEnoughDataException | Request.ParseFailedException e) {
            e.printStackTrace();
            chan.write(ByteBuffer.wrap("ERROR\r\n".getBytes()));
            continue;
          }
          logger.finest("Parsed request: " + req.toString());
          ByteBuffer rbuf = req.render();
          rbuf.position(0);
          int bytesWritten = asc.write(rbuf);
          logger.finest("Sent " + Integer.toString(bytesWritten) + " to server:");
          logger.finest(new String(rbuf.array()));
        }

        ByteBuffer bbuf2 = ByteBuffer.allocate(bufferSize);
        int bytesRead2 = asc.read(bbuf2);
        logger.finer("Input from Server:");
        logger.finer(Integer.toString(bytesRead2) + " bytes");
        if (bytesRead2 >= 0) {
          logger.finest(new String(bbuf2.array()));
          int bytesWritten2 = chan.write(ByteBuffer.wrap(bbuf2.array(), 0, bytesRead2)).get();
          logger.finer("Sent " + Integer.toString(bytesWritten2) + " to client");
        }

      }
    } catch (ExecutionException e) {
      e.printStackTrace();
    } catch (IOException | InterruptedException e) {
      e.printStackTrace();
      System.exit(1);
    } finally {
      try {
        if (asc != null) {
          asc.close();
        }
        if (chan != null) {
          chan.close();
        }
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  @Override
  public void failed(Throwable exc, Object attachment) {

  }
}
