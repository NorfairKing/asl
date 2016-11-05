package ch.ethz.asl.response;

import java.nio.ByteBuffer;
import java.util.Arrays;

import static ch.ethz.asl.generic_parsing.GenericParser.NEWLINE;
import static ch.ethz.asl.generic_parsing.GenericParser.SPACE;
import static ch.ethz.asl.response.response_parsing.ResponseParser.KEYWORD_END;
import static ch.ethz.asl.response.response_parsing.ResponseParser.KEYWORD_VALUE;

public class FoundValueResponse implements Response {
  private final byte[] key;
  private final byte[] flags;
  private final byte[] length;
  private final byte[] value;

  @Override
  public ByteBuffer render() {
    ByteBuffer bbuf =
        ByteBuffer.allocate(
            KEYWORD_VALUE.length
                + SPACE.length
                + this.key.length
                + SPACE.length
                + this.flags.length
                + SPACE.length
                + this.length.length
                + NEWLINE.length
                + this.value.length
                + NEWLINE.length
                + KEYWORD_END.length
                + NEWLINE.length);
    bbuf.put(KEYWORD_VALUE);
    bbuf.put(SPACE);
    bbuf.put(this.key);
    bbuf.put(SPACE);
    bbuf.put(this.flags);
    bbuf.put(SPACE);
    bbuf.put(this.length);
    bbuf.put(NEWLINE);
    bbuf.put(this.value);
    bbuf.put(NEWLINE);
    bbuf.put(KEYWORD_END);
    bbuf.put(NEWLINE);
    return bbuf;
  }

  @Override
  public String toString() {
    return new String(render().array());
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    FoundValueResponse that = (FoundValueResponse) o;

    if (!Arrays.equals(key, that.key)) return false;
    if (!Arrays.equals(flags, that.flags)) return false;
    if (!Arrays.equals(length, that.length)) return false;
    return Arrays.equals(value, that.value);
  }

  @Override
  public int hashCode() {
    int result = Arrays.hashCode(key);
    result = 31 * result + Arrays.hashCode(flags);
    result = 31 * result + Arrays.hashCode(length);
    result = 31 * result + Arrays.hashCode(value);
    return result;
  }

  public FoundValueResponse(byte[] key, byte[] flags, byte[] length, byte[] value) {
    this.key = key;
    this.flags = flags;
    this.length = length;
    this.value = value;
  }
}
