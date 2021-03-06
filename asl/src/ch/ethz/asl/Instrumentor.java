package ch.ethz.asl;

import ch.ethz.asl.request.RequestPacket;

import java.io.BufferedWriter;
import java.io.File;
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
  private static final String HEADER =
      "Kind,ReceivedTime,ParsedTime,EnqueuedTime,DequeuedTime,AskedTime,RepliedTime,RespondedTime";

  public Instrumentor(final String filepath, final int readSampleRate, final int writeSampleRate)
      throws IOException {
    this.file = filepath;
    this.readCounter = new Throttler(readSampleRate);
    this.writeCounter = new Throttler(writeSampleRate);

    File file = new File(filepath);
    file.getParentFile().mkdirs();
    FileWriter fstream = new FileWriter(file);
    writer = new BufferedWriter(fstream);
    writer.write(HEADER);
    writer.newLine();
    writer.flush();
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
      sb.append("FAILED");
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
      } catch (NoSuchElementException e) {
        sb = new StringBuilder("FAILED");
      }
    }
    synchronized (writer) {
      writer.write(sb.toString());
      writer.newLine();
      writer.flush();
    }
  }
}
