package eu.cssyd.asl.request;

import java.util.Arrays;

public class GetRequest extends Request {
  private final String[] keys;

  public GetRequest(final String... keys) {
    this.keys = keys;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    GetRequest that = (GetRequest) o;

    // Probably incorrect - comparing Object[] arrays with Arrays.equals
    return Arrays.equals(keys, that.keys);
  }

  @Override
  public int hashCode() {
    return Arrays.hashCode(keys);
  }

  @Override
  public String toString() {
    return "GetRequest{" +
        "keys=" + Arrays.toString(keys) +
        '}';
  }

  @Override
  public String render() {
    StringBuilder sb = new StringBuilder("get");
    for (String key : keys) {
      sb.append(" ")
          .append(key);
    }
    return sb.toString();
  }
}
