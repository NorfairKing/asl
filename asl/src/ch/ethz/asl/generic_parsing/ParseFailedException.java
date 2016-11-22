package ch.ethz.asl.generic_parsing;

import java.nio.ByteBuffer;

public class ParseFailedException extends IllegalArgumentException {
  private final ByteBuffer bbuff;

  public ParseFailedException(ByteBuffer bbuff) {
    this.bbuff = bbuff;
  }

  @Override
  public void printStackTrace() {
    super.printStackTrace();
    System.err.println(new String(bbuff.array()));
  }
}
