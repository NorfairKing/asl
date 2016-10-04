package ch.ethz.asl.request;

import com.sun.org.apache.regexp.internal.RE;

import java.nio.ByteBuffer;
import java.util.Arrays;

public class GetRequest implements Request {
  private final byte[] key;

  public GetRequest(byte[] key) {
    this.key = key;
  }

  @Override
  public ByteBuffer render() {
    ByteBuffer bbuf = ByteBuffer.allocate(Request.KEYWORD_GET.length + this.key.length + Request.NEWLINE.length);
    bbuf.put(Request.KEYWORD_GET);
    bbuf.put(this.key);
    bbuf.put(Request.NEWLINE);
    return bbuf;
  }

  @Override
  public String toString() {
    return "GetRequest{" +
        "key=" + Arrays.toString(key) +
        '}';
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    GetRequest that = (GetRequest) o;

    return Arrays.equals(key, that.key);

  }

  @Override
  public int hashCode() {
    return Arrays.hashCode(key);
  }

}
