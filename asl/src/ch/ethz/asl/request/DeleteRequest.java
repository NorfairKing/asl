package ch.ethz.asl.request;

import ch.ethz.asl.request.request_parsing.RequestParser;

import java.nio.ByteBuffer;

import static ch.ethz.asl.generic_parsing.GenericParser.NEWLINE;
import static ch.ethz.asl.generic_parsing.GenericParser.SPACE;
import static ch.ethz.asl.request.request_parsing.RequestParser.KEYWORD_DELETE;

public class DeleteRequest extends Request {
  public DeleteRequest(byte[] key) {
    super(key);
  }

  @Override
  public ByteBuffer render() {
    ByteBuffer bbuf =
        ByteBuffer.allocate(
            KEYWORD_DELETE.length + SPACE.length + this.key.length + NEWLINE.length);
    bbuf.put(KEYWORD_DELETE);
    bbuf.put(SPACE);
    bbuf.put(this.key);
    bbuf.put(NEWLINE);
    return bbuf;
  }

  @Override
  public RequestKind getKind() {
    return RequestKind.WRITE_REQUEST;
  }
}
