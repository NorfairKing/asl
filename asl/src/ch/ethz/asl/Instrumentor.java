package ch.ethz.asl;

import ch.ethz.asl.request.RequestPacket;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.NoSuchElementException;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Instrumentor {
  private final String file;
  private final BufferedWriter writer;
  private static final char COMMA = ',';
  private static final char NEWLINE = '\n';
  private final Throttler readCounter;
  private final Throttler writeCounter;
  private static final int READ_SAMPLE_SIZE = 1000;
  private static final int WRITE_SAMPLE_SIZE = READ_SAMPLE_SIZE / 100;
  private final Lock writerLock;

  public Instrumentor(final String file) throws IOException {
    this.file = file;
    this.readCounter = new Throttler(READ_SAMPLE_SIZE);
    this.writeCounter = new Throttler(WRITE_SAMPLE_SIZE);
    this.writerLock = new ReentrantLock();

    FileWriter fstream = new FileWriter(file);
    writer = new BufferedWriter(fstream);
    String header =
        "Kind,ReceivedTime,ParsedTime,EnqueuedTime,DequeuedTime,AskedTime,RepliedTime,RespondedTime"
            + NEWLINE;
    writer.write(header);
  }

  public static long now() {
    return System.nanoTime();
  }

  public boolean checkAndIncrement(RequestPacket packet) {
    switch (packet.getRequest().getKind()) {
      case READ_REQUEST:
        return readCounter.countAndCheck();
      case WRITE_REQUEST:
        return writeCounter.countAndCheck();
    }
    return false;
  }

  public void finaliseRequest(RequestPacket packet) throws IOException {
    if (!checkAndIncrement(packet)) {
      return;
    }
    StringBuilder sb = new StringBuilder();
    if (packet.hasAlreadyFailed()) {
      sb.append("FAILED\n");
    } else {
      try {
        sb.append(packet.getRequest().getKind().toString());
        sb.append(COMMA);
        sb.append(packet.getReceivedAt());
        sb.append(COMMA);
        sb.append(packet.getParsedAt());
        sb.append(COMMA);
        sb.append(packet.getEnqueuedAt());
        sb.append(COMMA);
        sb.append(packet.getDequeuedAt());
        sb.append(COMMA);
        sb.append(packet.getAskedAt());
        sb.append(COMMA);
        sb.append(packet.getRepliedAt());
        sb.append(COMMA);
        sb.append(packet.getRespondedAt());
        sb.append(NEWLINE);
      } catch (NoSuchElementException e) {
        sb = new StringBuilder("FAILED\n");
      }
    }
    writerLock.lock();
    writer.write(sb.toString());
    writer.flush();
    writerLock.unlock();
  }
}
