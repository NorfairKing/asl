package ch.ethz.asl.request;

import java.nio.ByteBuffer;

public abstract class Request {
  protected final byte[] key;

  public Request(final byte[] key) {
    this.key = key;
  }

  public abstract ByteBuffer render();

  @Override
  public String toString() {
    return new String(render().array());
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (!(o instanceof Request)) return false;

    Request request = (Request) o;

    return this.render().equals(request.render());
  }

  @Override
  public int hashCode() {
    return this.render().hashCode();
  }
}
