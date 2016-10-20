package ch.ethz.asl.response;

public enum SuccessType {
  SUCCESS,
  FAILURE;

  @Override
  public String toString() {
    switch (this) {
      case SUCCESS:
        return "success";
      case FAILURE:
        return "failure";
    }
    return null;
  }
}
