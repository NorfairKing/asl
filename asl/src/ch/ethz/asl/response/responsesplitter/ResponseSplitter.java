package ch.ethz.asl.response.responsesplitter;

import ch.ethz.asl.generic_parsing.GenericParser;
import ch.ethz.asl.generic_parsing.NotEnoughDataException;
import ch.ethz.asl.generic_parsing.ParseProgress;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

import static ch.ethz.asl.generic_parsing.GenericParser.NEWLINE;

public class ResponseSplitter {
  /**
   * This assumes that the given bytebuffer always contains the entire response and only the response to set requests.
   */
  public static List<ByteBuffer> splitResponses(ByteBuffer input) {
    List<ByteBuffer> result = new ArrayList<>();
    int offset = 0;
    while (true) {
      try {
        ParseProgress parseProgress = GenericParser.parseUntilNewline(input, offset);
        ByteBuffer bb = ByteBuffer.allocate(parseProgress.res.length + NEWLINE.length);
        bb.put(parseProgress.res);
        bb.put(NEWLINE);
        result.add(bb);
        offset = parseProgress.nextoffset;
      } catch (NotEnoughDataException e) {

        break;
      }
    }
    return result;
  }
}
