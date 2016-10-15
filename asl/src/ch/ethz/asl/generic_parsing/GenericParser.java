package ch.ethz.asl.generic_parsing;

import java.nio.ByteBuffer;

public class GenericParser {
  public static final byte[] NEWLINE = "\r\n".getBytes();
  public static final byte[] SPACE = " ".getBytes();

  public static ParseProgress parseUntilNewline(ByteBuffer byteBuffer, int offset) {
    int position = byteBuffer.position();
    byte[] res;
    int nextoff;
    for (int i = offset; true; i++) {
      if (position <= i + 1) {
        throw new NotEnoughDataException();
      }
      byte fst = byteBuffer.get(i);
      byte snd = byteBuffer.get(i + 1);
      if (fst == NEWLINE[0] && snd == NEWLINE[1]) {
        res = copyOver(byteBuffer, offset, i - offset);
        nextoff = i + 2;
        break;
      }
    }
    return new ParseProgress(res, nextoff);
  }

  public static ParseProgress parseUntilSpace(ByteBuffer byteBuffer, int offset) {
    int position = byteBuffer.position();
    byte[] res;
    int nextoff;
    for (int i = offset; true; i++) {
      if (position <= i) {
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

  public static byte[] copyOver(ByteBuffer byteBuffer, int off, int len) {
    byte[] res = new byte[len];
    for (int j = 0; j < len; j++) {
      res[j] = byteBuffer.get(j + off);
    }
    return res;
  }
}
