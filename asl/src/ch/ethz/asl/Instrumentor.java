package ch.ethz.asl;

import ch.ethz.asl.request.RequestPacket;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicLong;

public class Instrumentor {
  private final String file;
  private final BufferedWriter writer;
  private static final char COMMA = ',';
  private static final char NEWLINE = '\n';
  private AtomicLong counter;
  private static final int SAMPLE_SIZE = 1000;

  public Instrumentor(final String file) throws IOException {
    this.file = file;
    this.counter = new AtomicLong(0);

    FileWriter fstream = new FileWriter(file);
    writer = new BufferedWriter(fstream);
    String header =
        "ReceivedTime,ParsedTime,EnqueuedTime,DequeuedTime,AskedTime,RepliedTime,RespondedTime"
            + NEWLINE;
    writer.write(header);
  }

  public static long now() {
    return System.nanoTime();
  }

  public void finaliseRequest(RequestPacket packet) throws IOException {
    if (counter.getAndIncrement() % SAMPLE_SIZE != 0) {
      return;
    } // Only write stats of every so-manyth request.

    writer.write(Long.toString(packet.getReceivedAt()));
    writer.write(COMMA);
    writer.write(Long.toString(packet.getParsedAt()));
    writer.write(COMMA);
    writer.write(Long.toString(packet.getEnqueuedAt()));
    writer.write(COMMA);
    writer.write(Long.toString(packet.getDequeuedAt()));
    writer.write(COMMA);
    writer.write(Long.toString(packet.getAskedAt()));
    writer.write(COMMA);
    writer.write(Long.toString(packet.getRepliedAt()));
    writer.write(COMMA);
    writer.write(Long.toString(packet.getRespondedAt()));
    writer.write(NEWLINE);
    writer.flush();
  }
}
