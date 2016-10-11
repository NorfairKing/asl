package ch.ethz.asl.request;

import java.nio.ByteBuffer;
import java.util.Arrays;

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
    return Arrays.hashCode(this.render().array());
  }

  public abstract RequestKind getKind ();
}

