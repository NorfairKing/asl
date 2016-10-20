package ch.ethz.asl.response;

import java.util.Optional;

public class SevereServerErrorResponse extends ServerErrorResponse {
  private final Optional<String> message;

  public SevereServerErrorResponse() {
    this.message = Optional.empty();
  }

  public SevereServerErrorResponse(String message) {
    this.message = Optional.of(message);
  }
}
