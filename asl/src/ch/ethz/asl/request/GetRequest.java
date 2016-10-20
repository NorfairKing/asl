package ch.ethz.asl.request;

import ch.ethz.asl.request.request_parsing.RequestParser;

import java.nio.ByteBuffer;

import static ch.ethz.asl.generic_parsing.GenericParser.NEWLINE;
import static ch.ethz.asl.generic_parsing.GenericParser.SPACE;
import static ch.ethz.asl.request.request_parsing.RequestParser.KEYWORD_GET;

public class GetRequest extends Request {
  public GetRequest(byte[] key) {
    super(key);
  }

  @Override
  public ByteBuffer render() {
    ByteBuffer bbuf =
        ByteBuffer.allocate(KEYWORD_GET.length + SPACE.length + this.key.length + NEWLINE.length);
    bbuf.put(KEYWORD_GET);
    bbuf.put(SPACE);
    bbuf.put(this.key);
    bbuf.put(NEWLINE);
    return bbuf;
  }

  @Override
  public RequestKind getKind() {
    return RequestKind.READ_REQUEST;
  }
}
