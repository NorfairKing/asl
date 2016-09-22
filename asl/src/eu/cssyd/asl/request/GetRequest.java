package eu.cssyd.asl.request;

public class GetRequest extends Request {
  private final String key;

  public GetRequest(final String key) {
    this.key = key;
  }

  @Override
  public String toString() {
    return "GetRequest{" +
        "key='" + key + '\'' +
        '}';
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    GetRequest that = (GetRequest) o;

    return key != null ? key.equals(that.key) : that.key == null;

  }

  @Override
  public int hashCode() {
    return key != null ? key.hashCode() : 0;
  }

  @Override
  public String render() {
    return new StringBuilder()
        .append("get")
        .append(" ")
        .append(this.key)
        .toString();
  }
}
