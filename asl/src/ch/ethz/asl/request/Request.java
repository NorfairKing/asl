package ch.ethz.asl.request;

import java.nio.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Optional;

interface Request {

  String render();

  static final String NEWLINE = "\r\n";
  static final String KEYWORD_GET = "get ";

  static final ArrayList<String> KEYWORD_GET_PREFIXES = nonemptyPrefixes(KEYWORD_GET);

  // "get" -> ["g", "ge", "get"]
  static ArrayList<String> nonemptyPrefixes(String str) {
    ArrayList<String> result = new ArrayList<String>(str.length());
    for (int i = 0; i < str.length(); i++) {
      result.add(i, str.substring(0, i + 1));
    }
    return result;
  }

  static ParseResult parseRequest(ByteBuffer byteBuffer) {
    String str = new String(byteBuffer.array());
    if (str.isEmpty()) {
      return ParseResult.needsMoreData();
    }
    for (String prefix : KEYWORD_GET_PREFIXES) {
      if (str.equals(prefix)) {
        return ParseResult.needsMoreData();
      }
    }
    if (str.length() < KEYWORD_GET.length()) {
      return ParseResult.failed();
    }
    if (KEYWORD_GET.startsWith(str.substring(0, KEYWORD_GET.length()))) {
      if (!str.contains(NEWLINE)) {
        return ParseResult.needsMoreData();
      } else {
        String key = str.substring(KEYWORD_GET.length()).split(NEWLINE)[0];
        GetRequest result = new GetRequest(key);
        return ParseResult.done(result);
      }
    }
    return ParseResult.failed();
  }
}


enum ParseResultType {
  Done,
  NeedsMoreData,
  Failed
}

class ParseResult {
  public final ParseResultType type;
  public final Optional<Request> result;

  private ParseResult(ParseResultType type, Optional<Request> result) {
    this.type = type;
    this.result = result;
  }

  public static ParseResult done(Request result) {
    return new ParseResult(ParseResultType.Done, Optional.of(result));
  }

  public static ParseResult failed() {
    return new ParseResult(ParseResultType.Failed, Optional.empty());
  }

  public static ParseResult needsMoreData() {
    return new ParseResult(ParseResultType.NeedsMoreData, Optional.empty());
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    ParseResult that = (ParseResult) o;

    if (type != that.type) return false;
    return result != null ? result.equals(that.result) : that.result == null;

  }

  @Override
  public int hashCode() {
    int result1 = type != null ? type.hashCode() : 0;
    result1 = 31 * result1 + (result != null ? result.hashCode() : 0);
    return result1;
  }

  @Override
  public String toString() {
    return "ParseResult{" +
        "type=" + type +
        ", result=" + result +
        '}';
  }
}
