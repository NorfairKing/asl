package ch.ethz.asl.request;

import java.nio.ByteBuffer;

public class DeleteRequest implements Request {
  private final byte[] key;

  public DeleteRequest(byte[] key) {
    this.key = key;
  }

  @Override
  public ByteBuffer render() {
    ByteBuffer bbuf = ByteBuffer.allocate(
        Request.KEYWORD_DELETE.length
            + Request.SPACE.length
            + this.key.length
            + Request.NEWLINE.length);
    bbuf.put(Request.KEYWORD_DELETE);
    bbuf.put(Request.SPACE);
    bbuf.put(this.key);
    bbuf.put(Request.NEWLINE);
    return bbuf;
  }
}
