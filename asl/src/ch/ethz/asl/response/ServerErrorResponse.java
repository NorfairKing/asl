package ch.ethz.asl.response;

import ch.ethz.asl.request.request_parsing.RequestParser;

import java.nio.ByteBuffer;
import java.util.Optional;

import static ch.ethz.asl.generic_parsing.GenericParser.NEWLINE;
import static ch.ethz.asl.generic_parsing.GenericParser.SPACE;

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
    ByteBuffer res =
        ByteBuffer.allocate(
            SERVER_ERROR_STR.length + SPACE.length + messagePayload.length + NEWLINE.length);
    res.put(SERVER_ERROR_STR);
    res.put(SPACE);
    res.put(messagePayload);
    res.put(NEWLINE);
    return res;
  }

  private static final byte[] SERVER_ERROR_STR = "SERVER_ERROR".getBytes();

  @Override
  public String toString() {
    return new String(this.render().array());
  }
}
