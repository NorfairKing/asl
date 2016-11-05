package ch.ethz.asl.request;

import ch.ethz.asl.generic_parsing.NotEnoughDataException;
import ch.ethz.asl.generic_parsing.ParseFailedException;
import ch.ethz.asl.response.DeleteNotFoundResponse;
import ch.ethz.asl.response.FoundValueResponse;
import ch.ethz.asl.response.MissingValueResponse;
import ch.ethz.asl.response.StoredResponse;
import ch.ethz.asl.response.responsesplitter.DeletedResponse;
import org.junit.Test;

import java.nio.ByteBuffer;

import static ch.ethz.asl.request.request_parsing.RequestParser.parseRequest;
import static ch.ethz.asl.response.response_parsing.ResponseParser.parseResponse;
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
    public void parseFoundValueDone() {
        String s = "VALUE key 0 8\r\n12345678\r\nEND\r\n";
        assertThat(parseResponse(wrappingByteBuffer(s))).isEqualTo(new FoundValueResponse(
                "key".getBytes(),
                "0".getBytes(),
                "8".getBytes(),
                "12345678".getBytes()));
    }

    private ByteBuffer wrappingByteBuffer(String s) {
        ByteBuffer bbuf = ByteBuffer.wrap(s.getBytes());
        bbuf.position(s.length());
        parseResponse(bbuf);
        return bbuf;
    }
}
