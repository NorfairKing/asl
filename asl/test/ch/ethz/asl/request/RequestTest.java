package ch.ethz.asl.request;

import org.junit.Test;

import static com.google.common.truth.Truth.assertThat;

import java.nio.ByteBuffer;

public class RequestTest {
  @Test(expected = Request.NotEnoughDataException.class)
  public void parseRequestEmpty() {
    Request.parseRequest(ByteBuffer.wrap("".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseGetRequestPrefixG() {
    Request.parseRequest(ByteBuffer.wrap("g".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseGetRequestPrefixGE() {
    Request.parseRequest(ByteBuffer.wrap("ge".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseGetRequestPrefixGET() {
    Request.parseRequest(ByteBuffer.wrap("get".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseGetRequestPrefixGETSpace() {
    Request.parseRequest(ByteBuffer.wrap("get ".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseGetRequestNoNewline() {
    Request.parseRequest(ByteBuffer.wrap("get foo".getBytes()));
  }

  @Test(expected = Request.ParseFailedException.class)
  public void parseGetRequestFailedKeywordFirstLetter() {
    Request.parseRequest(ByteBuffer.wrap("a".getBytes()));
  }

  @Test(expected = Request.ParseFailedException.class)
  public void parseGetRequestFailedKeyword() {
    Request.parseRequest(ByteBuffer.wrap("gt".getBytes()));
  }

  @Test(expected = Request.ParseFailedException.class)
  public void parseGetRequestFailedFullKeyword() {
    Request.parseRequest(ByteBuffer.wrap("gte".getBytes()));
  }

  @Test(expected = Request.ParseFailedException.class)
  public void parseGetRequestFailedKeywordWithKey() {
    Request.parseRequest(ByteBuffer.wrap("gte key".getBytes()));
  }

  @Test(expected = Request.ParseFailedException.class)
  public void parseGetRequestFailedKeywordWithNewline() {
    Request.parseRequest(ByteBuffer.wrap("gte key\r\n".getBytes()));
  }

  @Test
  public void parseGetRequestDone() {
    assertThat(Request.parseRequest(ByteBuffer.wrap("get foo\r\n".getBytes())))
        .isEqualTo(new GetRequest("foo".getBytes()));
    assertThat(Request.parseRequest(ByteBuffer.wrap("get averyveryveryverylongkey\r\n".getBytes())))
        .isEqualTo(new GetRequest("averyveryveryverylongkey".getBytes()));
  }
}
