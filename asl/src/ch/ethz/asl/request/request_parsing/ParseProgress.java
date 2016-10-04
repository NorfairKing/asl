package ch.ethz.asl.request.request_parsing;

public class ParseProgress {
  public byte[] res;
  public int nextoffset;

  ParseProgress(byte[] res, int nextoffset) {
    this.res = res;
    this.nextoffset = nextoffset;
  }
}
