package ch.ethz.asl.request;

import ch.ethz.asl.request.request_parsing.RequestParser;

import java.nio.ByteBuffer;
import java.util.Arrays;

public class SetRequest extends Request {
  private final byte[] flags;
  private final byte[] exptime;
  private final byte[] length;
  private final byte[] value;

  public SetRequest(byte[] key, byte[] flags, byte[] exptime, byte[] length, byte[] value) {
    super(key);
    this.flags = flags;
    this.exptime = exptime;
    this.length = length;
    this.value = value;
  }

  @Override
  public ByteBuffer render() {
    ByteBuffer bbuf = ByteBuffer.allocate(
        RequestParser.KEYWORD_SET.length
            + RequestParser.SPACE.length
            + this.key.length
            + RequestParser.SPACE.length
            + this.flags.length
            + RequestParser.SPACE.length
            + this.exptime.length
            + RequestParser.SPACE.length
            + this.length.length
            + RequestParser.NEWLINE.length
            + this.value.length
            + RequestParser.NEWLINE.length);
    bbuf.put(RequestParser.KEYWORD_SET);
    bbuf.put(RequestParser.SPACE);
    bbuf.put(this.key);
    bbuf.put(RequestParser.SPACE);
    bbuf.put(this.flags);
    bbuf.put(RequestParser.SPACE);
    bbuf.put(this.exptime);
    bbuf.put(RequestParser.SPACE);
    bbuf.put(this.length);
    bbuf.put(RequestParser.NEWLINE);
    bbuf.put(this.value);
    bbuf.put(RequestParser.NEWLINE);
    return bbuf;
  }
}
