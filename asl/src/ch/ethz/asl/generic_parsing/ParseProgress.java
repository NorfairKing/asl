package ch.ethz.asl.generic_parsing;

public class ParseProgress {
  public final byte[] res;
  public final int nextoffset;

  public ParseProgress(final byte[] res, final int nextoffset) {
    this.res = res;
    this.nextoffset = nextoffset;
  }
}
