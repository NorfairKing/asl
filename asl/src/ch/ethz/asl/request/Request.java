package ch.ethz.asl.request;

import java.nio.ByteBuffer;

public abstract class Request {
  protected final byte[] key;

  public Request(byte[] key) {
    this.key = key;
  }

  static int keyHash(byte[] key) {
    return key.hashCode();
  }

  public abstract ByteBuffer render();
}
