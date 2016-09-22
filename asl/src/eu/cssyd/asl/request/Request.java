package eu.cssyd.asl.request;

import java.nio.*;
import java.util.Optional;

public abstract class Request {

  public abstract String render();

  public static Optional<Request> parseRequest(ByteBuffer byteBuffer) {
    String str = new String(byteBuffer.array());
    if (str.startsWith("get ")) {
      String keyssstr = str.substring("get ".length()).split("\r\n")[0];
      return Optional.of(new GetRequest(keyssstr));
      // String[] keys = keyssstr.split(" ");
    }
    return Optional.empty();
  }
}
