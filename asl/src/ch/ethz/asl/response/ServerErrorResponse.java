package ch.ethz.asl.response;

import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.Optional;

import static ch.ethz.asl.generic_parsing.GenericParser.NEWLINE;
import static ch.ethz.asl.generic_parsing.GenericParser.SPACE;
import static ch.ethz.asl.response.response_parsing.ResponseParser.KEYWORD_SERVER_ERROR;

public class ServerErrorResponse implements Response {
  private final Optional<byte[]> message;

  public ServerErrorResponse() {
    this.message = Optional.empty();
  }

  public ServerErrorResponse(byte[] message) {
    this.message = Optional.of(message);
  }

  public ServerErrorResponse(String message) {
    this.message = Optional.of(message.getBytes());
  }

  @Override
  public ByteBuffer render() {
    byte[] messagePayload;
    if (message.isPresent()) {
      messagePayload = message.get();
    } else {
      messagePayload = "unknown error".getBytes();
    }
    ByteBuffer res =
        ByteBuffer.allocate(
            KEYWORD_SERVER_ERROR.length + SPACE.length + messagePayload.length + NEWLINE.length);
    res.put(KEYWORD_SERVER_ERROR);
    res.put(SPACE);
    res.put(messagePayload);
    res.put(NEWLINE);
    return res;
  }

  @Override
  public boolean isWriteFailure() {
    return true;
  }

  @Override
  public String toString() {
    return new String(this.render().array());
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    ServerErrorResponse that = (ServerErrorResponse) o;

    return message != null
        ? (message.isPresent()
            ? Arrays.equals(message.get(), that.message.get())
            : message.equals(that.message))
        : that.message == null;
  }

  @Override
  public int hashCode() {
    return message != null ? message.hashCode() : 0;
  }
}
