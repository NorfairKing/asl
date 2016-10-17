package ch.ethz.asl;

import ch.ethz.asl.request.RequestPacket;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class Instrumentor {
    private final String file;
    private final BufferedWriter writer;
    private static final char COMMA = ',';
    private static final char NEWLINE = '\n';
    private final Throttler readCounter;
    private final Throttler writeCounter;
    private static final int READ_SAMPLE_SIZE = 1000;
    private static final int WRITE_SAMPLE_SIZE = READ_SAMPLE_SIZE / 100;

    public Instrumentor(final String file) throws IOException {
        this.file = file;
        this.readCounter = new Throttler(READ_SAMPLE_SIZE);
        this.writeCounter = new Throttler(WRITE_SAMPLE_SIZE);

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
        writer.write(packet.getRequest().getKind().toString());
        writer.write(COMMA);
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
