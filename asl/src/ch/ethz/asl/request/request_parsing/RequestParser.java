package ch.ethz.asl.request.request_parsing;

import ch.ethz.asl.request.DeleteRequest;
import ch.ethz.asl.request.GetRequest;
import ch.ethz.asl.request.Request;
import ch.ethz.asl.request.SetRequest;

import java.nio.ByteBuffer;

public class RequestParser {

  public static final byte[] NEWLINE = "\r\n".getBytes();
  public static final byte[] SPACE = " ".getBytes();
  public static final byte[] KEYWORD_GET = "get".getBytes();
  public static final byte[] KEYWORD_SET = "set".getBytes();
  public static final byte[] KEYWORD_DELETE = "delete".getBytes();

  public static Request parseRequest(ByteBuffer byteBuffer) throws NotEnoughDataException, ParseFailedException {
    int limit = byteBuffer.limit();

    if (limit <= 0) {
      throw new NotEnoughDataException();
    }

    switch (byteBuffer.get(0)) {
      case 'g':

        if (limit <= 1) {
          throw new NotEnoughDataException();
        }

        switch (byteBuffer.get(1)) {
          case 'e':

            if (limit <= 2) {
              throw new NotEnoughDataException();
            }

            switch (byteBuffer.get(2)) {
              case 't':

                if (limit <= 3) {
                  throw new NotEnoughDataException();
                }

                switch (byteBuffer.get(3)) {
                  case ' ':
                    return parseGetRequest(byteBuffer);
                  default:
                    throw new ParseFailedException();
                }
              default:
                throw new ParseFailedException();
            }
          default:
            throw new ParseFailedException();
        }
      case 's':

        if (limit <= 1) {
          throw new NotEnoughDataException();
        }

        switch (byteBuffer.get(1)) {
          case 'e':

            if (limit <= 2) {
              throw new NotEnoughDataException();
            }

            switch (byteBuffer.get(2)) {
              case 't':

                if (limit <= 3) {
                  throw new NotEnoughDataException();
                }

                switch (byteBuffer.get(3)) {
                  case ' ':
                    return parseSetRequest(byteBuffer);
                }
                break;
              default:
                throw new ParseFailedException();
            }
            break;
          default:
            throw new ParseFailedException();
        }
        break;
      case 'd':

        if (limit <= 1) {
          throw new NotEnoughDataException();
        }

        switch (byteBuffer.get(1)) {
          case 'e':

            if (limit <= 2) {
              throw new NotEnoughDataException();
            }

            switch (byteBuffer.get(2)) {
              case 'l':

                if (limit <= 3) {
                  throw new NotEnoughDataException();
                }

                switch (byteBuffer.get(3)) {
                  case 'e':
                    if (limit <= 4) {
                      throw new NotEnoughDataException();
                    }

                    switch (byteBuffer.get(4)) {
                      case 't':
                        if (limit <= 5) {
                          throw new NotEnoughDataException();
                        }

                        switch (byteBuffer.get(5)) {
                          case 'e':
                            if (limit <= 6) {
                              throw new NotEnoughDataException();
                            }

                            switch (byteBuffer.get(6)) {
                              case ' ':
                                return parseDeleteRequest(byteBuffer);
                              default:
                                throw new ParseFailedException();
                            }
                          default:
                            throw new ParseFailedException();
                        }
                      default:
                        throw new ParseFailedException();
                    }
                  default:
                    throw new ParseFailedException();
                }
              default:
                throw new ParseFailedException();
            }
          default:
            throw new ParseFailedException();
        }
      default:
        throw new ParseFailedException();
    }
    throw new ParseFailedException();
  }

  static GetRequest parseGetRequest(ByteBuffer byteBuffer) throws NotEnoughDataException, ParseFailedException {
    ParseProgress keyProgress = parseUntilNewline(byteBuffer, KEYWORD_SET.length + SPACE.length);
    byte[] key = keyProgress.res;
    return new GetRequest(key);
  }

  static SetRequest parseSetRequest(ByteBuffer byteBuffer) throws NotEnoughDataException, ParseFailedException {
    ParseProgress keyProgress = parseUntilSpace(byteBuffer, KEYWORD_SET.length + SPACE.length);
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

  static DeleteRequest parseDeleteRequest(ByteBuffer byteBuffer) throws NotEnoughDataException, ParseFailedException {
    ParseProgress keyProgress = parseUntilNewline(byteBuffer, KEYWORD_DELETE.length + SPACE.length);
    byte[] key = keyProgress.res;
    return new DeleteRequest(key);
  }

  static ParseProgress parseUntilNewline(ByteBuffer byteBuffer, int offset) {
    int limit = byteBuffer.limit();
    byte[] res;
    int nextoff;
    for (int i = offset; true; i++) {
      if (limit <= i + 1) {
        throw new NotEnoughDataException();
      }
      byte fst = byteBuffer.get(i);
      byte snd = byteBuffer.get(i + 1);
      if (fst == NEWLINE[0] && snd == NEWLINE[1]) {
        // 4 bytes for "get ", 2 for "\r\n"
        // [ ... , '\r', '\n', ...]
        //          i   i + 1
        res = copyOver(byteBuffer, offset, i - offset);
        nextoff = i + 2;
        break;
      }
    }
    return new ParseProgress(res, nextoff);
  }

  static ParseProgress parseUntilSpace(ByteBuffer byteBuffer, int offset) {
    int limit = byteBuffer.limit();
    byte[] res;
    int nextoff;
    for (int i = offset; true; i++) {
      if (limit <= i) {
        throw new NotEnoughDataException();
      }

      if (byteBuffer.get(i) == ' ') {
        res = copyOver(byteBuffer, offset, i - offset);
        nextoff = i + 1;
        break;
      }
    }
    return new ParseProgress(res, nextoff);
  }

  static byte[] copyOver(ByteBuffer byteBuffer, int off, int len) {
    byte[] res = new byte[len];
    for (int j = 0; j < len; j++) {
      res[j] = byteBuffer.get(j + off);
    }
    return res;
  }

}

