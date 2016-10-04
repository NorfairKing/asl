package ch.ethz.asl.request;

import ch.ethz.asl.request.request_parsing.NotEnoughDataException;
import ch.ethz.asl.request.request_parsing.ParseFailedException;
import org.junit.Test;

import java.nio.ByteBuffer;

import static ch.ethz.asl.request.request_parsing.RequestParser.parseRequest;
import static com.google.common.truth.Truth.assertThat;

public class RequestTest {
  @Test(expected = NotEnoughDataException.class)
  public void parseRequestEmpty() {
    parseRequest(ByteBuffer.wrap("".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestPrefixG() {
    parseRequest(ByteBuffer.wrap("g".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestPrefixGE() {
    parseRequest(ByteBuffer.wrap("ge".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestPrefixGET() {
    parseRequest(ByteBuffer.wrap("get".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestPrefixGETSpace() {
    parseRequest(ByteBuffer.wrap("get ".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestNoNewline() {
    parseRequest(ByteBuffer.wrap("get foo".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseGetRequestFailedKeywordFirstLetter() {
    parseRequest(ByteBuffer.wrap("a".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseGetRequestFailedKeyword() {
    parseRequest(ByteBuffer.wrap("gt".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseGetRequestFailedFullKeyword() {
    parseRequest(ByteBuffer.wrap("gte".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseGetRequestFailedKeywordWithKey() {
    parseRequest(ByteBuffer.wrap("gte key".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseGetRequestFailedKeywordWithNewline() {
    parseRequest(ByteBuffer.wrap("gte key\r\n".getBytes()));
  }

  @Test
  public void parseGetRequestDone() {
    assertThat(parseRequest(ByteBuffer.wrap("get foo\r\n".getBytes())))
        .isEqualTo(new GetRequest("foo".getBytes()));
    assertThat(parseRequest(ByteBuffer.wrap("get averyveryveryverylongkey\r\n".getBytes())))
        .isEqualTo(new GetRequest("averyveryveryverylongkey".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixS() {
    parseRequest(ByteBuffer.wrap("s".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSE() {
    parseRequest(ByteBuffer.wrap("se".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSET() {
    parseRequest(ByteBuffer.wrap("set".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpace() {
    parseRequest(ByteBuffer.wrap("set ".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKey() {
    parseRequest(ByteBuffer.wrap("set key".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlags() {
    parseRequest(ByteBuffer.wrap("set key 0".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptime() {
    parseRequest(ByteBuffer.wrap("set key 0 0".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptimeLength() {
    parseRequest(ByteBuffer.wrap("set key 0 0 8".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptimeLengthNewline() {
    parseRequest(ByteBuffer.wrap("set key 0 0 8\r\n".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptimeLengthNewlineTooShortData() {
    parseRequest(ByteBuffer.wrap("set key 0 0 8\r\n12345".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptimeLengthNewlineDataNoNewline() {
    parseRequest(ByteBuffer.wrap("set key 0 0 8\r\n12345678".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseSetRequestFailedKeywordFirstLetter() {
    parseRequest(ByteBuffer.wrap("b".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseSetRequestFailedKeyword() {
    parseRequest(ByteBuffer.wrap("st".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseSetRequestFailedFullKeyword() {
    parseRequest(ByteBuffer.wrap("ste".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseSetRequestFailedKeywordWithKey() {
    parseRequest(ByteBuffer.wrap("ste key".getBytes()));
  }

  @Test
  public void parseSetRequestDoneSimple() {
    assertThat(parseRequest(ByteBuffer.wrap("set foo 0 0 8\r\n12345678\r\n".getBytes())))
        .isEqualTo(new SetRequest("foo".getBytes(), "0".getBytes(), "0".getBytes(), "8".getBytes(), "12345678".getBytes()));
  }

  @Test
  public void parseSetRequestDoneLongerFlas() {
    assertThat(parseRequest(ByteBuffer.wrap("set aa 16 0 8\r\n12345678\r\n".getBytes())))
        .isEqualTo(new SetRequest("aa".getBytes(), "16".getBytes(), "0".getBytes(), "8".getBytes(), "12345678".getBytes()));
  }

  @Test
  public void parseSetRequestDoneLongerExptime() {
    assertThat(parseRequest(ByteBuffer.wrap("set foobar 0 20 4\r\nabcd\r\n".getBytes())))
        .isEqualTo(new SetRequest("foobar".getBytes(), "0".getBytes(), "20".getBytes(), "4".getBytes(), "abcd".getBytes()));
  }

  @Test
  public void parseSetRequestDoneLongerLength() {
    assertThat(parseRequest(ByteBuffer.wrap("set bbb 0 2 16\r\n0123456789abcdef\r\n".getBytes())))
        .isEqualTo(new SetRequest("bbb".getBytes(), "0".getBytes(), "2".getBytes(), "16".getBytes(), "0123456789abcdef".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixD() {
    parseRequest(ByteBuffer.wrap("d".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixDE() {
    parseRequest(ByteBuffer.wrap("de".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixDEL() {
    parseRequest(ByteBuffer.wrap("del".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixDELE() {
    parseRequest(ByteBuffer.wrap("dele".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixDELET() {
    parseRequest(ByteBuffer.wrap("delet".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixDELETE() {
    parseRequest(ByteBuffer.wrap("delete".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixDELETESpace() {
    parseRequest(ByteBuffer.wrap("delete ".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestNoNewline() {
    parseRequest(ByteBuffer.wrap("delete foo".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseDeleteRequestFailedKeywordFirstLetter() {
    parseRequest(ByteBuffer.wrap("c".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseDeleteRequestFailedKeyword() {
    parseRequest(ByteBuffer.wrap("dt".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseDeleteRequestFailedFullKeyword() {
    parseRequest(ByteBuffer.wrap("dte".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseDeleteRequestFailedKeywordWithKey() {
    parseRequest(ByteBuffer.wrap("dte key".getBytes()));
  }

  @Test(expected = ParseFailedException.class)
  public void parseDeleteRequestFailedKeywordWithNewline() {
    parseRequest(ByteBuffer.wrap("dte key\r\n".getBytes()));
  }
}
