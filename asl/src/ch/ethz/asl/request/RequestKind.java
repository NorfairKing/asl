package ch.ethz.asl.request;

public enum RequestKind {
  READ_REQUEST,
  WRITE_REQUEST;

  @Override
  public String toString() {
    switch (this) {
      case READ_REQUEST:
        return "read";
      case WRITE_REQUEST:
        return "write";
    }
    return super.toString();
  }
}
