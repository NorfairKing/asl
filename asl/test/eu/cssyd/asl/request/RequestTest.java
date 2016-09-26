package eu.cssyd.asl.request;

import org.junit.Test;

import static com.google.common.truth.Truth.assertThat;

import java.nio.ByteBuffer;

public class RequestTest {
  @Test
  public void parseRequestEmpty() {
    assertThat(Request.parseRequest(ByteBuffer.wrap("".getBytes())))
        .isEqualTo(ParseResult.needsMoreData());
  }

  @Test
  public void parseGetRequestSingleKeyDone() {
    assertThat(Request.parseRequest(ByteBuffer.wrap("get foo\r\n".getBytes())))
        .isEqualTo(ParseResult.done(new GetRequest("foo")));
    assertThat(Request.parseRequest(ByteBuffer.wrap("get averyveryveryverylongkey\r\n".getBytes())))
        .isEqualTo(ParseResult.done(new GetRequest("averyveryveryverylongkey")));
  }

  @Test
  public void parseGetRequestSingleKeyNeedsMoreData() {
    assertThat(Request.parseRequest(ByteBuffer.wrap("g".getBytes())))
        .isEqualTo(ParseResult.needsMoreData());
    assertThat(Request.parseRequest(ByteBuffer.wrap("ge".getBytes())))
        .isEqualTo(ParseResult.needsMoreData());
    assertThat(Request.parseRequest(ByteBuffer.wrap("get".getBytes())))
        .isEqualTo(ParseResult.needsMoreData());
    assertThat(Request.parseRequest(ByteBuffer.wrap("get foo".getBytes())))
        .isEqualTo(ParseResult.needsMoreData());
  }

  @Test
  public void parseGetRequestSingleKeyFailed() {
    assertThat(Request.parseRequest(ByteBuffer.wrap("gt".getBytes())))
        .isEqualTo(ParseResult.failed());
    assertThat(Request.parseRequest(ByteBuffer.wrap("gte".getBytes())))
        .isEqualTo(ParseResult.failed());
    assertThat(Request.parseRequest(ByteBuffer.wrap("gte key".getBytes())))
        .isEqualTo(ParseResult.failed());
    assertThat(Request.parseRequest(ByteBuffer.wrap("gte key\r\n".getBytes())))
        .isEqualTo(ParseResult.failed());
  }
}
