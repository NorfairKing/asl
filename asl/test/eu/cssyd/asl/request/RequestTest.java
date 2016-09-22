package eu.cssyd.asl.request;

import org.junit.Test;
import static com.google.common.truth.Truth.assertThat;

import java.nio.ByteBuffer;
import java.util.Optional;

public class RequestTest {
  @Test
  public void evaluatesExpression() {
    assertThat(Request.parseRequest(ByteBuffer.wrap("get foo".getBytes())))
        .isEqualTo(Optional.of(new GetRequest("foo")));
  }
}
