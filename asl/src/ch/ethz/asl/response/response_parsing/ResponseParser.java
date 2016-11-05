package ch.ethz.asl.response.response_parsing;

import ch.ethz.asl.generic_parsing.NotEnoughDataException;
import ch.ethz.asl.generic_parsing.ParseFailedException;
import ch.ethz.asl.generic_parsing.ParseProgress;
import ch.ethz.asl.response.*;
import ch.ethz.asl.response.responsesplitter.DeletedResponse;

import java.nio.ByteBuffer;

import static ch.ethz.asl.generic_parsing.GenericParser.*;

public class ResponseParser {

    public static final byte[] KEYWORD_STORED = "STORED".getBytes();
    public static final byte[] KEYWORD_VALUE = "VALUE".getBytes();
    public static final byte[] KEYWORD_END = "END".getBytes();
    public static final byte[] KEYWORD_DELETED = "DELETED".getBytes();
    public static final byte[] KEYWORD_NOT_FOUND = "NOT_FOUND".getBytes();

    /**
     * Position must be at end of string that was read in order to parse correctly.
     */
    public static Response parseResponse(ByteBuffer byteBuffer)
            throws NotEnoughDataException, ParseFailedException {
        int position = byteBuffer.position();

        if (position <= 0) {
            throw new NotEnoughDataException();
        }

        switch (byteBuffer.get(0)) {
            case 'V':
                ParseProgress vpp = parseLiteral(KEYWORD_VALUE, byteBuffer, 1, 1);
                ParseProgress vppsp = parseLiteral(SPACE, byteBuffer, 0, vpp.nextoffset);
                return parseFoundValueResponse(byteBuffer, vppsp.nextoffset);
            case 'S':
                ParseProgress spp = parseLiteral(KEYWORD_STORED, byteBuffer, 1, 1);
                return parseStoredResponse(byteBuffer, spp.nextoffset);
            case 'E':
                ParseProgress epp = parseLiteral(KEYWORD_END, byteBuffer, 1, 1);
                return parseMissingValueResponse(byteBuffer, epp.nextoffset);
            case 'D':
                ParseProgress dpp = parseLiteral(KEYWORD_DELETED, byteBuffer, 1, 1);
                return parseDeletedResponse(byteBuffer, dpp.nextoffset);
            case 'N':
                ParseProgress npp = parseLiteral(KEYWORD_NOT_FOUND, byteBuffer, 1, 1);
                return parseDeleteNotFoundResponse(byteBuffer, npp.nextoffset);
            default:
                throw new ParseFailedException();
        }
    }

    static FoundValueResponse parseFoundValueResponse(ByteBuffer byteBuffer, int offset){
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
        parseUntilNewline(byteBuffer, endOffset);
        return new FoundValueResponse(key, flags, length, value);

    }

    static StoredResponse parseStoredResponse(ByteBuffer byteBuffer, int offset)
            throws NotEnoughDataException, ParseFailedException {
        parseUntilNewline(byteBuffer, offset);
        return new StoredResponse();
    }

    static MissingValueResponse parseMissingValueResponse(ByteBuffer byteBuffer, int offset)
            throws NotEnoughDataException, ParseFailedException {
        parseUntilNewline(byteBuffer, offset);
        return new MissingValueResponse();
    }

    static DeletedResponse parseDeletedResponse(ByteBuffer byteBuffer, int offset)
            throws NotEnoughDataException, ParseFailedException {
        parseUntilNewline(byteBuffer, offset);
        return new DeletedResponse();
    }

    static DeleteNotFoundResponse parseDeleteNotFoundResponse(ByteBuffer byteBuffer, int offset)
            throws NotEnoughDataException, ParseFailedException {
        parseUntilNewline(byteBuffer, offset);
        return new DeleteNotFoundResponse();
    }
}
