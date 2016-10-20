package ch.ethz.asl.response;

import java.nio.ByteBuffer;

public interface Response {
  ByteBuffer render();
}
