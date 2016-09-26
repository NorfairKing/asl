package eu.cssyd.asl.request;

import org.junit.Test;
import static com.google.common.truth.Truth.assertThat;

import java.nio.ByteBuffer;
import java.util.Optional;

public class RequestTest {
  @Test
  public void parseGetRequestSingleKeyDone() {
    assertThat(Request.parseRequest(ByteBuffer.wrap("get foo\r\n".getBytes())))
        .isEqualTo(ParseResult.done(new GetRequest("foo")));
  }

  @Test
  public void parseGetRequestSingleKeyNeedsMoreData() {
    assertThat(Request.parseRequest(ByteBuffer.wrap("get foo".getBytes())))
        .isEqualTo(ParseResult.needsMoreData());
  }

  // @Test
  // public void parseGetRequestMultipleKeys() {
  //   assertThat(Request.parseRequest(ByteBuffer.wrap("get foo bar".getBytes())))
  //       .isEqualTo(Optional.of(new GetRequest("foo", "bar")));
  // }
}
