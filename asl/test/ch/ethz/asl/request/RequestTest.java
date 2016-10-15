package ch.ethz.asl.request;

import ch.ethz.asl.generic_parsing.NotEnoughDataException;
import ch.ethz.asl.generic_parsing.ParseFailedException;
import org.junit.Test;

import java.nio.ByteBuffer;

import static ch.ethz.asl.request.request_parsing.RequestParser.parseRequest;
import static com.google.common.truth.Truth.assertThat;

public class RequestTest {
  @Test(expected = NotEnoughDataException.class)
  public void parseRequestEmpty() {
    String s = "";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseRequestEmptyBiggerBuffer() {
    String s = "";
    ByteBuffer bbuf = ByteBuffer.allocate(s.length() + 5);
    bbuf.put(s.getBytes());
    parseRequest(bbuf);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestPrefixG() {
    String s = "g";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestPrefixGBiggerBuffer() {
    String s = "g";
    ByteBuffer bbuf = ByteBuffer.allocate(s.length() + 5);
    bbuf.put(s.getBytes());
    parseRequest(bbuf);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestPrefixGE() {
    String s = "ge";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestPrefixGEBiggerBuffer() {
    String s = "ge";
    ByteBuffer bbuf = ByteBuffer.allocate(s.length() + 5);
    bbuf.put(s.getBytes());
    parseRequest(bbuf);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestPrefixGET() {
    String s = "get";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestPrefixGETBiggerBuffer() {
    String s = "get";
    ByteBuffer bbuf = ByteBuffer.allocate(s.length() + 5);
    bbuf.put(s.getBytes());
    parseRequest(bbuf);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestPrefixGETSpace() {
    String s = "get ";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestPrefixGETSpaceBiggerBuffer() {
    String s = "get ";
    ByteBuffer bbuf = ByteBuffer.allocate(s.length() + 5);
    bbuf.put(s.getBytes());
    parseRequest(bbuf);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestNoNewline() {
    String s = "get foo";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestNoNewlineBiggerBuffer() {
    String s = "get foo";
    ByteBuffer bbuf = ByteBuffer.allocate(s.length() + 5);
    bbuf.put(s.getBytes());
    parseRequest(bbuf);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestUnfinishedNewline() {
    String s = "get foo\r";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseGetRequestUnfinishedNewlineBiggerBuffer() {
    String s = "get foo\r";
    ByteBuffer bbuf = ByteBuffer.allocate(s.length() + 5);
    bbuf.put(s.getBytes());
    parseRequest(bbuf);
  }

  @Test(expected = ParseFailedException.class)
  public void parseGetRequestFailedKeywordFirstLetter() {
    parseRequest(wrappingByteBuffer("a"));
  }

  @Test(expected = ParseFailedException.class)
  public void parseGetRequestFailedKeyword() {
    String s = "gt";
    wrappingByteBuffer(s);
  }

  @Test(expected = ParseFailedException.class)
  public void parseGetRequestFailedFullKeyword() {
    String s = "gte";
    wrappingByteBuffer(s);
  }

  @Test(expected = ParseFailedException.class)
  public void parseGetRequestFailedKeywordWithKey() {
    String s = "gte key";
    wrappingByteBuffer(s);
  }

  @Test(expected = ParseFailedException.class)
  public void parseGetRequestFailedKeywordWithNewline() {
    String s = "gte key\r\n";
    wrappingByteBuffer(s);
  }

  @Test
  public void parseGetRequestDone() {
    String s = "get foo\r\n";
    assertThat(parseRequest(wrappingByteBuffer(s))).isEqualTo(new GetRequest("foo".getBytes()));
    String s2 = "get averyveryveryverylongkey\r\n";
    assertThat(parseRequest(wrappingByteBuffer(s2)))
        .isEqualTo(new GetRequest("averyveryveryverylongkey".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixS() {
    String s = "s";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSE() {
    String s = "se";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSET() {
    String s = "set";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpace() {
    String s = "set ";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKey() {
    String s = "set key";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlags() {
    String s = "set key 0";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptime() {
    String s = "set key 0 0";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptimeLength() {
    String s = "set key 0 0 8";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptimeLengthNewline() {
    String s = "set key 0 0 8\r\n";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptimeLengthNewlineTooShortData() {
    String s = "set key 0 0 8\r\n12345";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptimeLengthNewlineDataNoNewline() {
    String s = "set key 0 0 8\r\n12345678";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseSetRequestPrefixSETSpaceKeyFlagsExptimeLengthNewlineDataUnfinishedNewline() {
    String s = "set key 0 0 8\r\n12345678\r";
    wrappingByteBuffer(s);
  }

  @Test(expected = ParseFailedException.class)
  public void parseSetRequestFailedKeywordFirstLetter() {
    String s = "b";
    wrappingByteBuffer(s);
  }

  @Test(expected = ParseFailedException.class)
  public void parseSetRequestFailedKeyword() {
    String s = "st";
    wrappingByteBuffer(s);
  }

  @Test(expected = ParseFailedException.class)
  public void parseSetRequestFailedFullKeyword() {
    String s = "ste";
    wrappingByteBuffer(s);
  }

  @Test(expected = ParseFailedException.class)
  public void parseSetRequestFailedKeywordWithKey() {
    String s = "ste key";
    wrappingByteBuffer(s);
  }

  @Test
  public void parseSetRequestDoneSimple() {
    String s = "set foo 0 0 8\r\n12345678\r\n";
    assertThat(parseRequest(wrappingByteBuffer(s)))
        .isEqualTo(
            new SetRequest(
                "foo".getBytes(),
                "0".getBytes(),
                "0".getBytes(),
                "8".getBytes(),
                "12345678".getBytes()));
  }

  @Test
  public void parseSetRequestDoneLongerFlas() {
    String s = "set aa 16 0 8\r\n12345678\r\n";
    assertThat(parseRequest(wrappingByteBuffer(s)))
        .isEqualTo(
            new SetRequest(
                "aa".getBytes(),
                "16".getBytes(),
                "0".getBytes(),
                "8".getBytes(),
                "12345678".getBytes()));
  }

  @Test
  public void parseSetRequestDoneLongerExptime() {
    String s = "set foobar 0 20 4\r\nabcd\r\n";
    assertThat(parseRequest(wrappingByteBuffer(s)))
        .isEqualTo(
            new SetRequest(
                "foobar".getBytes(),
                "0".getBytes(),
                "20".getBytes(),
                "4".getBytes(),
                "abcd".getBytes()));
  }

  @Test
  public void parseSetRequestDoneLongerLength() {
    String s = "set bbb 0 2 16\r\n0123456789abcdef\r\n";
    assertThat(parseRequest(wrappingByteBuffer(s)))
        .isEqualTo(
            new SetRequest(
                "bbb".getBytes(),
                "0".getBytes(),
                "2".getBytes(),
                "16".getBytes(),
                "0123456789abcdef".getBytes()));
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixD() {
    String s = "d";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixDE() {
    String s = "de";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixDEL() {
    String s = "del";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixDELE() {
    String s = "dele";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixDELET() {
    String s = "delet";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixDELETE() {
    String s = "delete";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestPrefixDELETESpace() {
    String s = "delete ";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseDeleteRequestNoNewline() {
    String s = "delete foo";
    wrappingByteBuffer(s);
  }

  @Test(expected = ParseFailedException.class)
  public void parseDeleteRequestFailedKeywordFirstLetter() {
    String s = "c";
    wrappingByteBuffer(s);
  }

  @Test(expected = ParseFailedException.class)
  public void parseDeleteRequestFailedKeyword() {
    String s = "dt";
    wrappingByteBuffer(s);
  }

  @Test(expected = ParseFailedException.class)
  public void parseDeleteRequestFailedFullKeyword() {
    String s = "dte";
    wrappingByteBuffer(s);
  }

  @Test(expected = ParseFailedException.class)
  public void parseDeleteRequestFailedKeywordWithKey() {
    String s = "dte key";
    wrappingByteBuffer(s);
  }

  @Test(expected = ParseFailedException.class)
  public void parseDeleteRequestFailedKeywordWithNewline() {
    String s = "dte key\r\n";
    wrappingByteBuffer(s);
  }

  private ByteBuffer wrappingByteBuffer(String s) {
    ByteBuffer bbuf = ByteBuffer.wrap(s.getBytes());
    bbuf.position(s.length());
    parseRequest(bbuf);
    return bbuf;
  }
}
