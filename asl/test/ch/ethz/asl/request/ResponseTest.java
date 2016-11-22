package ch.ethz.asl.request;

import ch.ethz.asl.generic_parsing.NotEnoughDataException;
import ch.ethz.asl.generic_parsing.ParseFailedException;
import ch.ethz.asl.response.*;
import com.google.common.collect.LinkedListMultimap;
import org.junit.Test;

import java.nio.ByteBuffer;
import java.util.LinkedList;

import static ch.ethz.asl.request.request_parsing.RequestParser.parseRequest;
import static ch.ethz.asl.response.response_parsing.ResponseParser.parseResponse;
import static ch.ethz.asl.response.response_parsing.ResponseParser.parseResponses;
import static com.google.common.truth.Truth.assertThat;

public class ResponseTest {
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
  public void parseRequestMissingValueMissingNewline() {
    String s = "END";
    wrappingByteBuffer(s);
  }

  @Test(expected = NotEnoughDataException.class)
  public void parseRequestFoundValueOnlyVALUE() {
    String s = "VALUE";
    wrappingByteBuffer(s);
  }

  @Test(expected = ParseFailedException.class)
  public void parseGetRequestFailedKeywordFirstLetter() {
    parseResponse(wrappingByteBuffer("X"));
  }

  @Test
  public void parseMissingValueDone() {
    String s = "END\r\n";
    assertThat(parseResponse(wrappingByteBuffer(s))).isEqualTo(new MissingValueResponse());
  }

  @Test
  public void parseStoredDone() {
    String s = "STORED\r\n";
    assertThat(parseResponse(wrappingByteBuffer(s))).isEqualTo(new StoredResponse());
  }

  @Test
  public void parseDeletedDone() {
    String s = "DELETED\r\n";
    assertThat(parseResponse(wrappingByteBuffer(s))).isEqualTo(new DeletedResponse());
  }

  @Test
  public void parseDeleteNotFoundDone() {
    String s = "NOT_FOUND\r\n";
    assertThat(parseResponse(wrappingByteBuffer(s))).isEqualTo(new DeleteNotFoundResponse());
  }

  @Test
  public void parseNotStoredDone() {
    String s = "NOT_STORED\r\n";
    assertThat(parseResponse(wrappingByteBuffer(s))).isEqualTo(new NotStoredResponse());
  }

  @Test
  public void parseServerErrorDone() {
    String s = "SERVER_ERROR hi\r\n";
    assertThat(parseResponse(wrappingByteBuffer(s))).isEqualTo(new ServerErrorResponse("hi"));
  }

  @Test
  public void parseErrorResponseDone() {
    String s = "ERROR\r\n";
    assertThat(parseResponse(wrappingByteBuffer(s))).isEqualTo(new ErrorResponse());
  }

  @Test
  public void parseFoundValueDone() {
    String s = "VALUE key 0 8\r\n12345678\r\nEND\r\n";
    assertThat(parseResponse(wrappingByteBuffer(s)))
        .isEqualTo(
            new FoundValueResponse(
                "key".getBytes(), "0".getBytes(), "8".getBytes(), "12345678".getBytes()));
  }

  @Test
  public void parseMultipleDifferentResponses() {
    String s = "STORED\r\nEND\r\nDELETED\r\n";
    LinkedList res = new LinkedList();
    res.add(new StoredResponse());
    res.add(new MissingValueResponse());
    res.add(new DeletedResponse());
    assertThat(parseResponses(wrappingByteBuffer(s))).isEqualTo(res);
  }

  private ByteBuffer wrappingByteBuffer(String s) {
    ByteBuffer bbuf = ByteBuffer.wrap(s.getBytes());
    bbuf.position(s.length());
    parseResponse(bbuf);
    return bbuf;
  }
}
