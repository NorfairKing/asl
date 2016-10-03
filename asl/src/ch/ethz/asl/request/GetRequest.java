package ch.ethz.asl.request;

public class GetRequest implements Request {
  private final String key;

  public GetRequest(String key) {
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
    return "get " + this.key;
  }
}
