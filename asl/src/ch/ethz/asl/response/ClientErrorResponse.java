package ch.ethz.asl.response;

import ch.ethz.asl.request.request_parsing.RequestParser;

import java.nio.ByteBuffer;
import java.util.Optional;

public class ClientErrorResponse implements Response {
  private final Optional<String> message;

  public ClientErrorResponse() {
    this.message = Optional.empty();
  }

  public ClientErrorResponse(String message) {
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
    ByteBuffer res =
        ByteBuffer.allocate(
            CLIENT_ERROR_STR.length
                + RequestParser.SPACE.length
                + messagePayload.length
                + RequestParser.NEWLINE.length);
    res.put(CLIENT_ERROR_STR);
    res.put(RequestParser.SPACE);
    res.put(messagePayload);
    res.put(RequestParser.NEWLINE);
    return res;
  }

  private static final byte[] CLIENT_ERROR_STR = "CLIENT_ERROR".getBytes();

  @Override
  public String toString() {
    return new String(this.render().array());
  }
}
