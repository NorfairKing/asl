package ch.ethz.asl;

import ch.ethz.asl.generic_parsing.NotEnoughDataException;
import ch.ethz.asl.generic_parsing.ParseFailedException;
import ch.ethz.asl.request.RequestPacket;
import ch.ethz.asl.response.Response;
import ch.ethz.asl.response.response_parsing.ResponseParser;

import java.io.IOException;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.*;
import java.util.logging.Logger;

import static ch.ethz.asl.Middleware.BUFFER_SIZE;

public class ServerWriteHandler {
    private final ServerHandler serverHandler;
    private final ServerAddress serverAddress;
    private static final Logger log = Logger.getGlobal();
    private final ExecutorService writeExecutor;
    private final ExecutorService readExecutor;
    private final BlockingQueue<RequestPacket> writequeue;
    private final BlockingQueue<RequestPacket> sentqueue;
    private AsynchronousSocketChannel connection;

    public ServerWriteHandler(ServerHandler serverHandler, final ServerAddress serverAddress) {
        this.serverHandler = serverHandler;
        this.serverAddress = serverAddress;
        writequeue = new LinkedBlockingQueue<>();
        sentqueue = new LinkedBlockingQueue<>();
        writeExecutor = Executors.newSingleThreadExecutor();
        readExecutor = Executors.newSingleThreadExecutor();
        this.connection = connect(serverAddress);
        if (this.connection == null) {
            shutdown();
            return;
        }
        writeExecutor.submit(new WriteWorker(this.connection));
        readExecutor.submit(new ReadWorker(this.connection));
    }

    private static AsynchronousSocketChannel connect(ServerAddress serverAddress) {
        SocketAddress address = serverAddress.getSocketAddress();
        AsynchronousSocketChannel serverConnection = null;
        try {
            serverConnection = AsynchronousSocketChannel.open();
            serverConnection.connect(address).get();
        } catch (IOException | InterruptedException | ExecutionException e) {
            e.printStackTrace();
        }
        return serverConnection;
    }

    void handle(final RequestPacket req) throws InterruptedException {
        writequeue.put(req);
        req.setEnqueued();
    }

    private void shutdown() {
        serverHandler.shutdown();
    }

    private class WriteWorker implements Runnable {
        private final AsynchronousSocketChannel serverConnection;

        WriteWorker(final AsynchronousSocketChannel serverConnection) {
            this.serverConnection = serverConnection;
        }

        @Override
        public void run() {
            Thread.currentThread().setName("WriteWorker for write requests to server " + serverAddress);
            while (true) {
                if (ServerWriteHandler.this.serverHandler.isShuttingDown()) {
                    log.info("Shutting down write worker for server: " + serverAddress.getSocketAddress());
                    return; // Stop
                }
                handleOneRequest();
            }
        }

        private void handleOneRequest() {
            RequestPacket packet = null;
            try {
                packet = writequeue.take();
                packet.setDequeued();
            } catch (InterruptedException e) {
                e.printStackTrace();
                log.severe(
                        "Write worker for server: "
                                + serverAddress.getSocketAddress()
                                + " was interrupted while waiting for requests.");
                return;
            }
            sendWrite(packet);
            packet.setAsked();
            try {
                sentqueue.put(packet);
            } catch (InterruptedException e) {
                e.printStackTrace();
                log.severe(
                        "Write worker for server: "
                                + serverAddress.getSocketAddress()
                                + " was interrupted adding a request to the sent queue.");
            }
        }

        private void sendWrite(final RequestPacket req) {
            ByteBuffer rbuf = req.getRequest().render();
            rbuf.position(0);
            int bytesWritten;
            try {
                bytesWritten = serverConnection.write(rbuf).get();
            } catch (InterruptedException | ExecutionException e) {
                e.printStackTrace();
                log.severe("Exception while trying to write to server " + serverAddress);
                shutdown();
                return;
            }
            if (bytesWritten <= 0) {
                log.severe("Wrote " + bytesWritten + " bytes to server " + serverAddress);
                shutdown();
            }
        }
    }

    private class ReadWorker implements Runnable {
        private final AsynchronousSocketChannel serverConnection;
        private final ReadCompletionHandler handler;

        private ReadWorker(final AsynchronousSocketChannel serverConnection) {
            this.serverConnection = serverConnection;
            this.handler = new ReadCompletionHandler();
        }

        @Override
        public void run() {
            Thread.currentThread().setName("Readworker for write requests to server " + serverAddress);
            spin();
        }

        private void spin() {
            ByteBuffer bbuf = ByteBuffer.allocate((sentqueue.size() + 1) * BUFFER_SIZE);
            serverConnection.read(bbuf, bbuf, handler);
        }

        private class ReadCompletionHandler implements CompletionHandler<Integer, ByteBuffer> {

            @Override
            public void completed(Integer bytesRead, ByteBuffer bbuf) {
                if (ServerWriteHandler.this.serverHandler.isShuttingDown()) {
                    return;
                }

                if (bytesRead <= 0) {
                    shutdown();
                    return;
                }

                List<Response> resps = new LinkedList<>();
                try {
                    resps = ResponseParser.parseResponses(bbuf);
                } catch (NotEnoughDataException | ParseFailedException e) {
                    e.printStackTrace(); // FIXME handle this case.
                }

                assert (sentqueue.size() >= resps.size());
                for (Response resp : resps) {
                    // It's the response to the first request (hopefully)
                    RequestPacket packet;
                    try {
                        packet = sentqueue.take();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                        log.severe(
                                "Write worker for server: "
                                        + serverAddress.getSocketAddress()
                                        + " was interrupted while taking packets out of the sent queue.");
                        shutdown();
                        return;
                    }
                    // Send the response
                    try {
                        packet.respond(resp);
                    } catch (InterruptedException | ExecutionException | IOException e) {
                        e.printStackTrace(); // FIXME handle this somehow
                    }
                }
                // Restart the waiting for the reader.
                spin();
            }

            @Override
            public void failed(Throwable throwable, ByteBuffer bbuf) {
                throwable.printStackTrace();
                shutdown();
            }
        }
    }
}
