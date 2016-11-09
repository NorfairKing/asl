package ch.ethz.asl.request.request_parsing;

import ch.ethz.asl.generic_parsing.NotEnoughDataException;
import ch.ethz.asl.generic_parsing.ParseFailedException;
import ch.ethz.asl.generic_parsing.ParseProgress;
import ch.ethz.asl.request.DeleteRequest;
import ch.ethz.asl.request.GetRequest;
import ch.ethz.asl.request.Request;
import ch.ethz.asl.request.SetRequest;

import java.nio.ByteBuffer;

import static ch.ethz.asl.generic_parsing.GenericParser.*;

public class RequestParser {

  public static final byte[] KEYWORD_GET = "get".getBytes();
  public static final byte[] KEYWORD_SET = "set".getBytes();
  public static final byte[] KEYWORD_DELETE = "delete".getBytes();

  public static Request parseRequest(ByteBuffer byteBuffer)
      throws NotEnoughDataException, ParseFailedException {
    int position = byteBuffer.position();

    if (position <= 0) {
      throw new NotEnoughDataException();
    }

    switch (byteBuffer.get(0)) {
      case 'g':
        ParseProgress getpp = parseLiteral(KEYWORD_GET, byteBuffer, 1, 1);
        ParseProgress getspp = parseLiteral(SPACE, byteBuffer, 0, getpp.nextoffset);
        return parseGetRequest(byteBuffer, getspp.nextoffset);
      case 's':
        ParseProgress setpp = parseLiteral(KEYWORD_SET, byteBuffer, 1, 1);
        ParseProgress setspp = parseLiteral(SPACE, byteBuffer, 0, setpp.nextoffset);
        return parseSetRequest(byteBuffer, setspp.nextoffset);
      case 'd':
        ParseProgress delpp = parseLiteral(KEYWORD_DELETE, byteBuffer, 1, 1);
        ParseProgress delspp = parseLiteral(SPACE, byteBuffer, 0, delpp.nextoffset);
        return parseDeleteRequest(byteBuffer, delspp.nextoffset);
      default:
        throw new ParseFailedException(byteBuffer);
    }
  }

  static GetRequest parseGetRequest(ByteBuffer byteBuffer, int offset)
      throws NotEnoughDataException, ParseFailedException {
    ParseProgress keyProgress = parseUntilNewline(byteBuffer, offset);
    byte[] key = keyProgress.res;
    return new GetRequest(key);
  }

  static SetRequest parseSetRequest(ByteBuffer byteBuffer, int offset)
      throws NotEnoughDataException, ParseFailedException {
    ParseProgress keyProgress = parseUntilSpace(byteBuffer, offset);
    byte[] key = keyProgress.res;
    int keyOffset = keyProgress.nextoffset;
    ParseProgress flagsProgress = parseUntilSpace(byteBuffer, keyOffset);
    byte[] flags = flagsProgress.res;
    int flagsOffset = flagsProgress.nextoffset;
    ParseProgress exptimeProgress = parseUntilSpace(byteBuffer, flagsOffset);
    byte[] exptime = exptimeProgress.res;
    int exptimeOffset = exptimeProgress.nextoffset;
    ParseProgress lengthProgress = parseUntilNewline(byteBuffer, exptimeOffset);
    byte[] length = lengthProgress.res;
    int lengthOffset = lengthProgress.nextoffset;
    ParseProgress valueProgress = parseUntilNewline(byteBuffer, lengthOffset);
    byte[] value = valueProgress.res;
    return new SetRequest(key, flags, exptime, length, value);
  }

  static DeleteRequest parseDeleteRequest(ByteBuffer byteBuffer, int offset)
      throws NotEnoughDataException, ParseFailedException {
    ParseProgress keyProgress = parseUntilNewline(byteBuffer, offset);
    byte[] key = keyProgress.res;
    return new DeleteRequest(key);
  }
}
