package ch.ethz.asl;

import java.net.InetSocketAddress;
import java.net.SocketAddress;

public class ServerAddress {
  private final InetSocketAddress socketAddress;

  public ServerAddress(String straddres) {

    String[] splitt = straddres.split(":");
    String serverUrl = splitt[0];
    int serverPort = Integer.parseInt(splitt[1]);
    socketAddress = new InetSocketAddress(serverUrl, serverPort);
  }

  public SocketAddress getSocketAddress() {
    return socketAddress;
  }
}
