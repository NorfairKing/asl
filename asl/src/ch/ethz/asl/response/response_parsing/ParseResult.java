package ch.ethz.asl.response.response_parsing;

import ch.ethz.asl.response.Response;

public class ParseResult {
    public final Response resp;
    public final int nextOffset;

    public ParseResult(final Response resp, final int nextOffset) {
        this.resp = resp;
        this.nextOffset = nextOffset;
    }
}
