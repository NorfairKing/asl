package ch.ethz.asl.request;

import java.nio.ByteBuffer;

// TODO implement delete as well!
public interface Request {

  ByteBuffer render();

  byte[] NEWLINE = "\r\n".getBytes();
  byte[] SPACE = " ".getBytes();
  byte[] KEYWORD_GET = "get ".getBytes();
  byte[] KEYWORD_SET = "set ".getBytes();

  class ParseFailedException extends IllegalArgumentException {
  }

  class NotEnoughDataException extends IllegalArgumentException {
  }


  static Request parseRequest(ByteBuffer byteBuffer) throws NotEnoughDataException, ParseFailedException {
    int limit = byteBuffer.limit();

    if (limit <= 3)
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
      default:
        throw new ParseFailedException();
    }
    throw new ParseFailedException();
  }

  static GetRequest parseGetRequest(ByteBuffer byteBuffer) throws NotEnoughDataException, ParseFailedException {
    int limit = byteBuffer.limit();
    ParseProgress keyProgress = parseUntilNewline(byteBuffer, Request.KEYWORD_SET.length);
    byte[] key = keyProgress.res;
    return new GetRequest(key);
  }

  static SetRequest parseSetRequest(ByteBuffer byteBuffer) throws NotEnoughDataException, ParseFailedException {
    ParseProgress keyProgress = parseUntilSpace(byteBuffer, Request.KEYWORD_SET.length);
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

  class ParseProgress {
    public byte[] res;
    public int nextoffset;

    ParseProgress(byte[] res, int nextoffset) {
      this.res = res;
      this.nextoffset = nextoffset;
    }
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
        // ['g', 'e', 't', ' ',     ... here be a key ... , '\r', '\n', ...]
        //  0   1    2    3    4
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
