package ch.ethz.asl.response.response_parsing;

import ch.ethz.asl.generic_parsing.NotEnoughDataException;
import ch.ethz.asl.generic_parsing.ParseFailedException;
import ch.ethz.asl.generic_parsing.ParseProgress;
import ch.ethz.asl.response.*;
import ch.ethz.asl.response.DeletedResponse;

import java.nio.ByteBuffer;
import java.util.LinkedList;
import java.util.List;

import static ch.ethz.asl.generic_parsing.GenericParser.*;

public class ResponseParser {

  public static final byte[] KEYWORD_STORED = "STORED".getBytes();
  public static final byte[] KEYWORD_VALUE = "VALUE".getBytes();
  public static final byte[] KEYWORD_END = "END".getBytes();
  public static final byte[] KEYWORD_DELETED = "DELETED".getBytes();
  public static final byte[] KEYWORD_NOT_FOUND = "NOT_FOUND".getBytes();
  public static final byte[] KEYWORD_ERROR = "ERROR".getBytes();

  /**
   * Position must be at end of string that was read in order to parse correctly.
   */
  public static Response parseResponse(ByteBuffer byteBuffer)
      throws NotEnoughDataException, ParseFailedException {
    ParseResult pr = parseSingleInternal(byteBuffer, 0);
    return pr.resp;
  }

  public static List<Response> parseResponses(ByteBuffer byteBuffer)
      throws NotEnoughDataException, ParseFailedException {
    int position = byteBuffer.position();
    List<Response> responses = new LinkedList<Response>();
    int curOffset = 0;
    while (curOffset < position) {
      ParseResult pr = parseSingleInternal(byteBuffer, curOffset);
      responses.add(pr.resp);
      curOffset = pr.nextOffset;
    }
    return responses;
  }

  private static ParseResult parseSingleInternal(ByteBuffer byteBuffer, int offset)
      throws NotEnoughDataException, ParseFailedException {
    int position = byteBuffer.position();

    if (position <= offset) {
      throw new NotEnoughDataException();
    }

    switch (byteBuffer.get(offset)) {
      case 'V':
        ParseProgress vpp = parseLiteral(KEYWORD_VALUE, byteBuffer, 1, offset + 1);
        ParseProgress vppsp = parseLiteral(SPACE, byteBuffer, 0, vpp.nextoffset);
        return parseFoundValueResponse(byteBuffer, vppsp.nextoffset);
      case 'S':
        ParseProgress spp = parseLiteral(KEYWORD_STORED, byteBuffer, 1, offset + 1);
        return parseStoredResponse(byteBuffer, spp.nextoffset);
      case 'E':
        if (position <= offset) {
          throw new NotEnoughDataException();
        }
        switch (byteBuffer.get(offset + 1)) {
          case 'N':
            ParseProgress epp = parseLiteral(KEYWORD_END, byteBuffer, 2, offset + 2);
            return parseMissingValueResponse(byteBuffer, epp.nextoffset);
          case 'R':
            ParseProgress errpp = parseLiteral(KEYWORD_ERROR, byteBuffer, 2, offset + 2);
            return parseErrorResponse(byteBuffer, errpp.nextoffset);
          default:
            throw new ParseFailedException();
        }
      case 'D':
        ParseProgress dpp = parseLiteral(KEYWORD_DELETED, byteBuffer, 1, offset + 1);
        return parseDeletedResponse(byteBuffer, dpp.nextoffset);
      case 'N':
        ParseProgress npp = parseLiteral(KEYWORD_NOT_FOUND, byteBuffer, 1, offset + 1);
        return parseDeleteNotFoundResponse(byteBuffer, npp.nextoffset);
      default:
        throw new ParseFailedException();
    }
  }

  static ParseResult parseFoundValueResponse(ByteBuffer byteBuffer, int offset) {
    ParseProgress keyProgress = parseUntilSpace(byteBuffer, offset);
    byte[] key = keyProgress.res;
    int keyOffset = keyProgress.nextoffset;
    ParseProgress flagsProgress = parseUntilSpace(byteBuffer, keyOffset);
    byte[] flags = flagsProgress.res;
    int flagsOffset = flagsProgress.nextoffset;
    ParseProgress lengthProgress = parseUntilNewline(byteBuffer, flagsOffset);
    byte[] length = lengthProgress.res;
    int lengthOffset = lengthProgress.nextoffset;
    ParseProgress valueProgress = parseUntilNewline(byteBuffer, lengthOffset);
    byte[] value = valueProgress.res;
    int valueOffset = valueProgress.nextoffset;
    ParseProgress endProgress = parseLiteral(KEYWORD_END, byteBuffer, 0, valueOffset);
    int endOffset = endProgress.nextoffset;
    ParseProgress finalProgress = parseUntilNewline(byteBuffer, endOffset);
    int finalOffset = finalProgress.nextoffset;
    return new ParseResult(new FoundValueResponse(key, flags, length, value), finalOffset);
  }

  static ParseResult parseStoredResponse(ByteBuffer byteBuffer, int offset)
      throws NotEnoughDataException, ParseFailedException {
    ParseProgress finalProgress = parseUntilNewline(byteBuffer, offset);
    int finalOffset = finalProgress.nextoffset;
    return new ParseResult(new StoredResponse(), finalOffset);
  }

  static ParseResult parseMissingValueResponse(ByteBuffer byteBuffer, int offset)
      throws NotEnoughDataException, ParseFailedException {
    ParseProgress finalProgress = parseUntilNewline(byteBuffer, offset);
    int finalOffset = finalProgress.nextoffset;
    return new ParseResult(new MissingValueResponse(), finalOffset);
  }

  static ParseResult parseErrorResponse(ByteBuffer byteBuffer, int offset)
      throws NotEnoughDataException, ParseFailedException {
    ParseProgress finalProgress = parseUntilNewline(byteBuffer, offset);
    int finalOffset = finalProgress.nextoffset;
    return new ParseResult(new ErrorResponse(), finalOffset);
  }

  static ParseResult parseDeletedResponse(ByteBuffer byteBuffer, int offset)
      throws NotEnoughDataException, ParseFailedException {
    ParseProgress finalProgress = parseUntilNewline(byteBuffer, offset);
    int finalOffset = finalProgress.nextoffset;
    return new ParseResult(new DeletedResponse(), finalOffset);
  }

  static ParseResult parseDeleteNotFoundResponse(ByteBuffer byteBuffer, int offset)
      throws NotEnoughDataException, ParseFailedException {
    ParseProgress finalProgress = parseUntilNewline(byteBuffer, offset);
    int finalOffset = finalProgress.nextoffset;
    return new ParseResult(new DeleteNotFoundResponse(), finalOffset);
  }
}
