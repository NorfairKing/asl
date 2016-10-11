package ch.ethz.asl;

import ch.ethz.asl.request.RequestPacket;

import java.util.concurrent.ExecutionException;

public class ServerHandler {
  private final ServerWriteHandler serverWriteHandler;
  private final ServerReadHandler serverReadHandler;

  public ServerHandler(final ServerAddress serverAddress) {
    this.serverWriteHandler = new ServerWriteHandler(serverAddress);
    this.serverReadHandler = new ServerReadHandler(serverAddress);
  }

  public void handle(final RequestPacket req) throws ExecutionException, InterruptedException {
    switch (req.getRequest().getKind()) {
      case READ_REQUEST:
        serverReadHandler.handle(req);
        break;
      case WRITE_REQUEST:
        serverWriteHandler.handle(req);
        break;
    }
  }
}
