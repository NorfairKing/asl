package ch.ethz.asl;

import ch.ethz.asl.request.Request;
import ch.ethz.asl.request.RequestPacket;
import ch.ethz.asl.response.Response;
import ch.ethz.asl.response.ServerErrorResponse;
import ch.ethz.asl.response.SuccessfulResponse;
import ch.ethz.asl.response.responsesplitter.ResponseSplitter;

import java.io.IOException;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import java.nio.channels.SocketChannel;
import java.util.List;
import java.util.concurrent.*;
import java.util.function.BiFunction;
import java.util.logging.Logger;

import static ch.ethz.asl.Middleware.BUFFER_SIZE;

public class ServerWriteHandler {
    private final ServerHandler serverHandler;
    private final ServerAddress serverAddress;
    private static final Logger log = Logger.getGlobal();
    private final ExecutorService executor;
    private final BlockingQueue<RequestPacket> writequeue;
    private final BlockingQueue<RequestPacket> sentqueue;

    public ServerWriteHandler(ServerHandler serverHandler, final ServerAddress serverAddress) {
        this.serverHandler = serverHandler;
        this.serverAddress = serverAddress;
        writequeue = new LinkedBlockingQueue<>();
        sentqueue = new LinkedBlockingQueue<>();
        executor = Executors.newSingleThreadExecutor();
        executor.submit(new WriteWorker());
    }

    public void handle(final RequestPacket req) throws InterruptedException {
        log.finest("Putting request on write queue for server " + serverAddress + ", now sized " + writequeue.size());
        writequeue.put(req);
        log.fine("Put request on write queue, for server " + serverAddress + ", now sized " + writequeue.size());
    }

    private void shutdown() {
        serverHandler.shutdown();
    }

    class WriteWorker implements Runnable {
        private AsynchronousSocketChannel serverConnection;

        public WriteWorker() {
            connect();
        }

        private void connect() {
            SocketAddress address = serverAddress.getSocketAddress();
            log.fine("Writer connecting to: " + address);
            serverConnection = null;
            try {
                serverConnection = AsynchronousSocketChannel.open();
                serverConnection.connect(address).get();
                log.fine("Writer connected to: " + address);
            } catch (IOException | InterruptedException | ExecutionException e) {
                e.printStackTrace();
                shutdown();
            }
            startReader();
        }

        private void startReader() {
            new ReadCompletionHandler(serverConnection).startReader();
        }

        @Override
        public void run() {
            while (true) {
                if (ServerWriteHandler.this.serverHandler.isShuttingDown()) {
                    log.fine("Shutting down write worker for server: " + serverAddress.getSocketAddress());
                    return; // Stop
                }
                handleOneRequest();
            }
        }

        private void handleOneRequest() {
            RequestPacket packet = null;
            try {
                log.fine("Dequeuing request from writequeue for server " + serverAddress + ", now sized " + writequeue.size());
                packet = writequeue.take();
                log.fine("Dequeud request from writequeue, for server " + serverAddress + ", now sized " + writequeue.size());
                packet.setDequeued();
            } catch (InterruptedException e) {
                e.printStackTrace();
                log.fine(
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
                log.fine(
                        "Write worker for server: "
                                + serverAddress.getSocketAddress()
                                + " was interrupted adding a request to the sent queue.");
                return;
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
                log.fine("Exception while trying to write to server " + serverAddress.getSocketAddress());
                shutdown();
                return;
            }
            if (bytesWritten <= 0) {
                log.fine("Wrote " + bytesWritten + " bytes to server " + serverAddress.getSocketAddress());
                shutdown();
                return;
            }
            log.finest("Sent " + bytesWritten + " to server:");
            log.finest(new String(rbuf.array()));
        }
    }

    class ReadCompletionHandler implements CompletionHandler<Integer, ByteBuffer> {
        private final AsynchronousSocketChannel serverConnection;

        public ReadCompletionHandler(final AsynchronousSocketChannel serverConnection) {
            this.serverConnection = serverConnection;
        }

        @Override
        public void completed(Integer bytesRead, ByteBuffer bbuf) {
            if (ServerWriteHandler.this.serverHandler.isShuttingDown()) {
                log.fine(
                        "Shutting down writer read completion handler for server: "
                                + serverAddress.getSocketAddress());
                return;
            }

            if (bytesRead <= 0) {
                log.fine("Read " + bytesRead + " bytes from server " + serverAddress.getSocketAddress());
                shutdown();
                return;
            }
            log.finest("Input from Server:");
            log.finest(bytesRead + " bytes");
            byte[] dst = new byte[bytesRead];
            bbuf.position(0);
            bbuf.get(dst);
            ByteBuffer stripped = ByteBuffer.wrap(dst);
            stripped.position(bytesRead);
            List<ByteBuffer> resps = ResponseSplitter.splitResponses(stripped);

            for (ByteBuffer bb : resps) {
                // It's the response to the first request (hopefully)
                RequestPacket packet;
                try {
                    packet = sentqueue.take();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                    log.fine(
                            "Write worker for server: "
                                    + serverAddress.getSocketAddress()
                                    + " was interrupted while taking packets out of the sent queue.");
                    shutdown();
                    return;
                }
                // Make the response.
                Response resp = new SuccessfulResponse(bb);
                // Send the response
                try {
                    packet.respond(resp);
                } catch (InterruptedException | ExecutionException | IOException e) {
                    e.printStackTrace(); // FIXME handle this somehow
                }
            }
            startReader();
        }

        private void startReader() {
            ByteBuffer bbuf = ByteBuffer.allocate((sentqueue.size() + 1) * BUFFER_SIZE);
            serverConnection.read(bbuf, bbuf, this);
        }

        @Override
        public void failed(Throwable throwable, ByteBuffer bbuf) {
        }
    }
}
