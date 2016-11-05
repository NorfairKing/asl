package ch.ethz.asl.response.responsesplitter;

import ch.ethz.asl.response.Response;

import java.nio.ByteBuffer;

import static ch.ethz.asl.generic_parsing.GenericParser.NEWLINE;
import static ch.ethz.asl.response.response_parsing.ResponseParser.KEYWORD_NOT_FOUND;

public class DeletedResponse implements Response {

    @Override
    public ByteBuffer render() {
        ByteBuffer bbuff = ByteBuffer.allocate(KEYWORD_NOT_FOUND.length + NEWLINE.length);
        bbuff.put(KEYWORD_NOT_FOUND);
        bbuff.put(NEWLINE);
        return bbuff;
    }

    @Override
    public String toString() {
        return new String(render().array());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return true;
    }
}
