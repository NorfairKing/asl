package ch.ethz.asl.request;

import ch.ethz.asl.request.request_parsing.RequestParser;

import java.nio.ByteBuffer;
import java.util.Arrays;

import static ch.ethz.asl.generic_parsing.GenericParser.NEWLINE;
import static ch.ethz.asl.generic_parsing.GenericParser.SPACE;
import static ch.ethz.asl.request.request_parsing.RequestParser.KEYWORD_SET;

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
    ByteBuffer bbuf =
        ByteBuffer.allocate(
            KEYWORD_SET.length
                + SPACE.length
                + this.key.length
                + SPACE.length
                + this.flags.length
                + SPACE.length
                + this.exptime.length
                + SPACE.length
                + this.length.length
                + NEWLINE.length
                + this.value.length
                + NEWLINE.length);
    bbuf.put(KEYWORD_SET);
    bbuf.put(SPACE);
    bbuf.put(this.key);
    bbuf.put(SPACE);
    bbuf.put(this.flags);
    bbuf.put(SPACE);
    bbuf.put(this.exptime);
    bbuf.put(SPACE);
    bbuf.put(this.length);
    bbuf.put(NEWLINE);
    bbuf.put(this.value);
    bbuf.put(NEWLINE);
    return bbuf;
  }

  @Override
  public RequestKind getKind() {
    return RequestKind.WRITE_REQUEST;
  }
}
