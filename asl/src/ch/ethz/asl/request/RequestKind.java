package ch.ethz.asl.request;

public enum RequestKind {
  READ_REQUEST,
  WRITE_REQUEST;

  @Override
  public String toString() {
    switch(this){
      case READ_REQUEST:
        return "READ";
      case WRITE_REQUEST:
        return "WRITE";
    }
    return super.toString();
  }
}
