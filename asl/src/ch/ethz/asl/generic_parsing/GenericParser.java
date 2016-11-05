package ch.ethz.asl.generic_parsing;

import java.nio.ByteBuffer;

public class GenericParser {
    public static final byte[] NEWLINE = "\r\n".getBytes();
    public static final byte[] SPACE = " ".getBytes();

    public static ParseProgress parseLiteral(byte[] bytes, ByteBuffer byteBuffer, int bytesOffset, int offset) throws NotEnoughDataException, ParseFailedException {
        int position = byteBuffer.position();
        int ix = 0;
        for (int i = bytesOffset; i < bytes.length; i++) {
            int bbpos = offset + ix;
            if (position <= bbpos) {
                throw new NotEnoughDataException();
            }
            byte bi = byteBuffer.get(bbpos);
            if (bi != bytes[i]) {
                throw new ParseFailedException();
            }
            ix++;
        }
        return new ParseProgress(bytes, offset + bytes.length - bytesOffset);
    }

    public static ParseProgress parseUntilNewline(ByteBuffer byteBuffer, int offset) throws NotEnoughDataException {
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

    public static ParseProgress parseUntilSpace(ByteBuffer byteBuffer, int offset) throws NotEnoughDataException {
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

    public static byte[] copyOver(ByteBuffer byteBuffer, int off, int len) throws NotEnoughDataException {
        byte[] res = new byte[len];
        for (int j = 0; j < len; j++) {
            res[j] = byteBuffer.get(j + off);
        }
        return res;
    }
}
