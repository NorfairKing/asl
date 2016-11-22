package ch.ethz.asl.response;

import java.nio.ByteBuffer;

import static ch.ethz.asl.generic_parsing.GenericParser.NEWLINE;
import static ch.ethz.asl.response.response_parsing.ResponseParser.KEYWORD_NOT_STORED;
import static ch.ethz.asl.response.response_parsing.ResponseParser.KEYWORD_STORED;

public class NotStoredResponse implements Response {
  @Override
  public ByteBuffer render() {
    ByteBuffer bbuff = ByteBuffer.allocate(KEYWORD_NOT_STORED.length + NEWLINE.length);
    bbuff.put(KEYWORD_NOT_STORED);
    bbuff.put(NEWLINE);
    return bbuff;
  }

  @Override
  public boolean isWriteFailure() {
    return true;
  }

  @Override
  public String toString() {
    return new String(render().array());
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    return true;
  }
}