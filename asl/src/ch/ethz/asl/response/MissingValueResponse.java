package ch.ethz.asl.response;

import java.nio.ByteBuffer;

import static ch.ethz.asl.generic_parsing.GenericParser.NEWLINE;
import static ch.ethz.asl.response.response_parsing.ResponseParser.KEYWORD_END;

public class MissingValueResponse implements Response {

    @Override
    public ByteBuffer render() {
        ByteBuffer bbuff = ByteBuffer.allocate(KEYWORD_END.length + NEWLINE.length);
        bbuff.put(KEYWORD_END);
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
