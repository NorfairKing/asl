package ch.ethz.asl.generic_parsing;

public class ParseProgress {
  public byte[] res;
  public int nextoffset;

  public ParseProgress(byte[] res, int nextoffset) {
    this.res = res;
    this.nextoffset = nextoffset;
  }
}
