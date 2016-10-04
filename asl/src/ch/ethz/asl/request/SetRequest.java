package ch.ethz.asl.request;

import java.nio.ByteBuffer;
import java.util.Arrays;

public class SetRequest implements Request {
  private final byte[] key;
  private final byte[] flags;
  private final byte[] exptime;
  private final byte[] length;
  private final byte[] value;

  public SetRequest(byte[] key, byte[] flags, byte[] exptime, byte[] length, byte[] value) {
    this.key = key;
    this.flags = flags;
    this.exptime = exptime;
    this.length = length;
    this.value = value;
  }

  @Override
  public ByteBuffer render() {
    ByteBuffer bbuf = ByteBuffer.allocate(
        Request.KEYWORD_SET.length
            + this.key.length
            + this.flags.length
            + this.exptime.length
            + this.length.length
            + Request.NEWLINE.length
            + this.value.length
            + Request.NEWLINE.length);
    bbuf.put(Request.KEYWORD_SET);
    bbuf.put(this.key);
    bbuf.put(Request.SPACE);
    bbuf.put(this.flags);
    bbuf.put(Request.SPACE);
    bbuf.put(this.exptime);
    bbuf.put(Request.SPACE);
    bbuf.put(this.length);
    bbuf.put(Request.NEWLINE);
    bbuf.put(this.value);
    bbuf.put(Request.NEWLINE);
    return bbuf;
  }

  @Override
  public String toString() {
    return "SetRequest{" +
        "key=" + Arrays.toString(key) +
        ", flags=" + Arrays.toString(flags) +
        ", exptime=" + Arrays.toString(exptime) +
        ", length=" + Arrays.toString(length) +
        ", value=" + Arrays.toString(value) +
        '}';
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    SetRequest that = (SetRequest) o;

    if (!Arrays.equals(key, that.key)) return false;
    if (!Arrays.equals(flags, that.flags)) return false;
    if (!Arrays.equals(exptime, that.exptime)) return false;
    if (!Arrays.equals(length, that.length)) return false;
    return Arrays.equals(value, that.value);

  }

  @Override
  public int hashCode() {
    int result = Arrays.hashCode(key);
    result = 31 * result + Arrays.hashCode(flags);
    result = 31 * result + Arrays.hashCode(exptime);
    result = 31 * result + Arrays.hashCode(length);
    result = 31 * result + Arrays.hashCode(value);
    return result;
  }
}
