package ch.ethz.asl.response;

import ch.ethz.asl.request.Request;
import ch.ethz.asl.request.request_parsing.RequestParser;

import java.nio.ByteBuffer;
import java.util.Optional;

public class ServerErrorResponse implements Response {
  private final Optional<String> message;

  public ServerErrorResponse() {
    this.message = Optional.empty();
  }

  public ServerErrorResponse(String message) {
    this.message = Optional.of(message);
  }

  @Override
  public ByteBuffer render() {
    byte[] messagePayload;
    if (message.isPresent()) {
      messagePayload = message.get().getBytes();
    } else {
      messagePayload = "unknown error".getBytes();
    }
    ByteBuffer res = ByteBuffer.allocate(
        SERVER_ERROR_STR.length
            + RequestParser.SPACE.length
            + messagePayload.length
            + RequestParser.NEWLINE.length
    );
    res.put(SERVER_ERROR_STR);
    res.put(RequestParser.SPACE);
    res.put(messagePayload);
    res.put(RequestParser.NEWLINE);
    return res;
  }

  private static final byte[] SERVER_ERROR_STR = "SERVER_ERROR".getBytes();
}
