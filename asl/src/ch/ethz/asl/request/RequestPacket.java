package ch.ethz.asl.request;

import ch.ethz.asl.Instrumentor;
import ch.ethz.asl.response.Response;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import static ch.ethz.asl.Instrumentor.*;

public class RequestPacket {
  private static final Logger log = Logger.getGlobal();
  private final AsynchronousSocketChannel chan;
  private final Request req;

  private final Instrumentor instrumentor;

  private Optional<Long> receivedTime;
  private Optional<Long> parsedTime;
  private Optional<Long> enqueuedTime;
  private Optional<Long> dequeuedTime;
  private Optional<Long> askedTime;
  private Optional<Long> repliedTime;
  private Optional<Long> respondedTime;
  private boolean alreadyFailed;
  private AtomicInteger replicationCounter;

  public RequestPacket(
      final AsynchronousSocketChannel chan, final Request req, final Instrumentor instrumentor) {
    this.chan = chan;
    this.req = req;
    this.instrumentor = instrumentor;
    this.replicationCounter = new AtomicInteger(1);
    this.receivedTime = Optional.empty();
    this.parsedTime = Optional.empty();
    this.enqueuedTime = Optional.empty();
    this.dequeuedTime = Optional.empty();
    this.askedTime = Optional.empty();
    this.repliedTime = Optional.empty();
    this.respondedTime = Optional.empty();
    this.alreadyFailed = false;
  }

  public void respond(Response res) throws InterruptedException, ExecutionException, IOException {
    if (replicationCounter.decrementAndGet() > 0) {
      return;
    }
    setReplied();
    RequestPacket.respond(this.chan, res);
    setResponded();
    instrumentor.finaliseRequest(this);
  }

  public static void respond(final AsynchronousSocketChannel chan, final Response res)
      throws InterruptedException, ExecutionException {
    //    log.finest("Produced response: " + res.toString());

    if (!chan.isOpen()) {
      log.info("Not writing to client connection because it is closed.");
      return;
    }

    ByteBuffer responseBytes = res.render();
    responseBytes.position(0);
    int bytesWritten2 = chan.write(responseBytes).get();
    //    log.finest("Sent " + Integer.toString(bytesWritten2) + " to client");
    if (bytesWritten2 <= 0) {
      log.info("Failed to write response to client, closing connection.");
      try {
        chan.close();
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  public long getReceivedAt() {
    return receivedTime.get();
  }

  public void setReceivedAt(long time) {
    if (receivedTime.isPresent()) {
      return;
    }
    receivedTime = Optional.of(time);
  }

  public long getParsedAt() {
    return parsedTime.get();
  }

  public void setParsed() {
    if (parsedTime.isPresent()) {
      return;
    }
    parsedTime = Optional.of(now());
  }

  public long getEnqueuedAt() {
    return enqueuedTime.get();
  }

  public void setEnqueued() {
    if (enqueuedTime.isPresent()) {
      return;
    }
    enqueuedTime = Optional.of(now());
  }

  public long getDequeuedAt() {
    return dequeuedTime.get();
  }

  public void setDequeued() {
    if (dequeuedTime.isPresent()) {
      return;
    }
    dequeuedTime = Optional.of(now());
  }

  public long getAskedAt() {
    return askedTime.get();
  }

  public void setAsked() {
    if (askedTime.isPresent()) {
      return;
    }
    askedTime = Optional.of(now());
  }

  public long getRepliedAt() {
    return repliedTime.get();
  }

  public void setReplied() {
    if (repliedTime.isPresent()) {
      return;
    }
    repliedTime = Optional.of(now());
  }

  public long getRespondedAt() {
    return respondedTime.get();
  }

  private void setResponded() {
    if (respondedTime.isPresent()) {
      return;
    }
    respondedTime = Optional.of(now());
  }

  public void setFailed() {
    alreadyFailed = true;
  }

  public boolean hasAlreadyFailed() {
    return alreadyFailed;
  }

  public Request getRequest() {
    return req;
  }

  public void setReplicationCounter(int size) {
    this.replicationCounter.set(size);
  }
}
