package ch.ethz.asl.request;

import java.nio.ByteBuffer;

public interface Request {

  ByteBuffer render();

  byte[] NEWLINE = "\r\n".getBytes();
  byte[] KEYWORD_GET = "get ".getBytes();

  // ArrayList<String> KEYWORD_GET_PREFIXES = nonemptyPrefixes(KEYWORD_GET);

  // // "get" -> ["g", "ge", "get"]
  // static ArrayList<byte[]> nonemptyPrefixes(byte[] str) {
  //   ArrayList<byte[]> result = new ArrayList(str.length);
  //   for (int i = 0; i < str.length; i++) {
  //     result.add(Arrays.copyOfRange(str, 0, i + 1));
  //   }
  //   return result;
  // }

  class ParseFailedException extends IllegalArgumentException {
  }

  class NotEnoughDataException extends IllegalArgumentException {
  }


  static Request parseRequest(ByteBuffer byteBuffer) throws NotEnoughDataException, ParseFailedException {
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

                    for (int i = 4; true; i++) {
                      if (limit <= i + 1) {
                        throw new NotEnoughDataException();
                      }
                      byte fst = byteBuffer.get(i);
                      byte snd = byteBuffer.get(i + 1);
                      if (fst == NEWLINE[0] && snd == NEWLINE[1]) {
                        // 4 bytes for "get ", 2 for "\r\n"
                        // ['g', 'e', 't', ' ',     ... here be a key ... , '\r', '\n', ...]
                        //  0   1    2    3    4
                        int len = i - 4;
                        int off = 4;
                        byte[] key = new byte[len];

                        for (int j = 0; j < len; j++) {
                          key[j] = byteBuffer.get(j + off);
                        }
                        return new GetRequest(key);
                      }
                    }
                  default:
                }
              default:
            }
          default:
        }
      default:
        throw new ParseFailedException();
    }
  }
}
