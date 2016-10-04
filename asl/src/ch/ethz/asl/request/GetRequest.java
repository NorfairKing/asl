package ch.ethz.asl.request;

import ch.ethz.asl.request.request_parsing.RequestParser;

import java.nio.ByteBuffer;

public class GetRequest extends Request {
  public GetRequest(byte[] key) {
    super(key);
  }

  @Override
  public ByteBuffer render() {
    ByteBuffer bbuf = ByteBuffer.allocate(
        RequestParser.KEYWORD_GET.length
            + RequestParser.SPACE.length
            + this.key.length
            + RequestParser.NEWLINE.length);
    bbuf.put(RequestParser.KEYWORD_GET);
    bbuf.put(RequestParser.SPACE);
    bbuf.put(this.key);
    bbuf.put(RequestParser.NEWLINE);
    return bbuf;
  }
}
