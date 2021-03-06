package ch.ethz.asl.response;

import ch.ethz.asl.request.request_parsing.RequestParser;

import java.nio.ByteBuffer;

import static ch.ethz.asl.generic_parsing.GenericParser.NEWLINE;

public class ErrorResponse implements Response {

  @Override
  public ByteBuffer render() {
    ByteBuffer res = ByteBuffer.allocate(ERROR_STR.length + NEWLINE.length);
    res.put(ERROR_STR);
    res.put(NEWLINE);
    return res;
  }

  @Override
  public boolean isWriteFailure() {
    return true;
  }

  private static final byte[] ERROR_STR = "ERROR".getBytes();

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    return true;
  }

  @Override
  public String toString() {
    return new String(this.render().array());
  }
}
