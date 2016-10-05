package ch.ethz.asl.response;

import ch.ethz.asl.request.request_parsing.RequestParser;

import java.nio.ByteBuffer;

public class ErrorResponse implements Response {

  @Override
  public ByteBuffer render() {
    ByteBuffer res = ByteBuffer.allocate(ERROR_STR.length + RequestParser.NEWLINE.length);
    res.put(ERROR_STR);
    res.put(RequestParser.NEWLINE);
    return res;
  }

  private static final byte[] ERROR_STR = "ERROR".getBytes();

  @Override
  public String toString() {
    return new String(this.render().array());
  }
}
