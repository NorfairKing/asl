package ch.ethz.asl.response;

import java.nio.ByteBuffer;

public class SuccessfulResponse implements Response {
  private final ByteBuffer payload;

  public SuccessfulResponse(ByteBuffer payload) {
    this.payload = payload;
  }

  @Override
  public ByteBuffer render() {
    return payload;
  }
}
