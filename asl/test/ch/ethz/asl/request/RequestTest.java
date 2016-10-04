package ch.ethz.asl.request;

import org.junit.Test;

import java.nio.ByteBuffer;

import static com.google.common.truth.Truth.assertThat;

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

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseSetRequestPrefixS() {
    Request.parseRequest(ByteBuffer.wrap("s".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseSetRequestPrefixSE() {
    Request.parseRequest(ByteBuffer.wrap("se".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseSetRequestPrefixSET() {
    Request.parseRequest(ByteBuffer.wrap("set".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpace() {
    Request.parseRequest(ByteBuffer.wrap("set ".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKey() {
    Request.parseRequest(ByteBuffer.wrap("set key".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlags() {
    Request.parseRequest(ByteBuffer.wrap("set key 0".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptime() {
    Request.parseRequest(ByteBuffer.wrap("set key 0 0".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptimeLength() {
    Request.parseRequest(ByteBuffer.wrap("set key 0 0 8".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptimeLengthNewline() {
    Request.parseRequest(ByteBuffer.wrap("set key 0 0 8\r\n".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptimeLengthNewlineTooShortData() {
    Request.parseRequest(ByteBuffer.wrap("set key 0 0 8\r\n12345".getBytes()));
  }

  @Test(expected = Request.NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptimeLengthNewlineDataNoNewline() {
    Request.parseRequest(ByteBuffer.wrap("set key 0 0 8\r\n12345678".getBytes()));
  }

  @Test(expected = Request.ParseFailedException.class)
  public void parseSetRequestFailedKeywordFirstLetter() {
    Request.parseRequest(ByteBuffer.wrap("b".getBytes()));
  }

  @Test(expected = Request.ParseFailedException.class)
  public void parseSetRequestFailedKeyword() {
    Request.parseRequest(ByteBuffer.wrap("st".getBytes()));
  }

  @Test(expected = Request.ParseFailedException.class)
  public void parseSetRequestFailedFullKeyword() {
    Request.parseRequest(ByteBuffer.wrap("ste".getBytes()));
  }

  @Test(expected = Request.ParseFailedException.class)
  public void parseSetRequestFailedKeywordWithKey() {
    Request.parseRequest(ByteBuffer.wrap("ste key".getBytes()));
  }

  @Test
  public void parseSetRequestDoneSimple() {
    assertThat(Request.parseRequest(ByteBuffer.wrap("set foo 0 0 8\r\n12345678\r\n".getBytes())))
        .isEqualTo(new SetRequest("foo".getBytes(), "0".getBytes(), "0".getBytes(), "8".getBytes(), "12345678".getBytes()));
  }

  @Test
  public void parseSetRequestDoneLongerFlas() {
    assertThat(Request.parseRequest(ByteBuffer.wrap("set aa 16 0 8\r\n12345678\r\n".getBytes())))
        .isEqualTo(new SetRequest("aa".getBytes(), "16".getBytes(), "0".getBytes(), "8".getBytes(), "12345678".getBytes()));
  }

  @Test
  public void parseSetRequestDoneLongerExptime() {
    assertThat(Request.parseRequest(ByteBuffer.wrap("set foobar 0 20 4\r\nabcd\r\n".getBytes())))
        .isEqualTo(new SetRequest("foobar".getBytes(), "0".getBytes(), "20".getBytes(), "4".getBytes(), "abcd".getBytes()));
  }

  @Test
  public void parseSetRequestDoneLongerLength() {
    assertThat(Request.parseRequest(ByteBuffer.wrap("set bbb 0 2 16\r\n0123456789abcdef\r\n".getBytes())))
        .isEqualTo(new SetRequest("bbb".getBytes(), "0".getBytes(), "2".getBytes(), "16".getBytes(), "0123456789abcdef".getBytes()));
  }
}
