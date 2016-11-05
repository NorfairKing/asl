package ch.ethz.asl;

import ch.ethz.asl.request.RequestPacket;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

public class RunMW {

  private static final Logger logger = Logger.getGlobal();
  static String myIp = null;
  static int myPort = 0;
  static List<String> mcAddresses = null;
  static int numThreadsPTP = -1;
  static int writeToCount = -1;
  static Level verbosity = Level.INFO;
  static String logfile = "trace.csv";
  static int readSampleRate = 1000;
  static int writeSampleRate = 1000;

  public static void main(String[] args) throws Exception {
    // -----------------------------------------------------------------------------
    // Parse and prepare arguments
    // -----------------------------------------------------------------------------

    parseArguments(args);

    // -----------------------------------------------------------------------------
    // Start the Middleware
    // -----------------------------------------------------------------------------

    logger.setLevel(verbosity);
    logger.addHandler(new AdhocLogger());
    new Middleware(myIp, myPort, mcAddresses, numThreadsPTP, writeToCount, logfile, readSampleRate, writeSampleRate).run();
  }

  private static void parseArguments(String[] args) {
    Map<String, List<String>> params = new HashMap<>();

    List<String> options = null;
    for (int i = 0; i < args.length; i++) {
      final String a = args[i];

      if (a.charAt(0) == '-') {
        if (a.length() < 2) {
          System.err.println("Error at argument " + a);
          System.exit(1);
        }

        options = new ArrayList<String>();
        params.put(a.substring(1), options);
      } else if (options != null) {
        options.add(a);
      } else {
        System.err.println("Illegal parameter usage");
        System.exit(1);
      }
    }

    if (params.size() == 0) {
      printUsageWithError(null);
    }

    if (params.get("l") != null) {
      myIp = params.get("l").get(0);
    } else {
      printUsageWithError("Provide this machine's external IP! (see ifconfig or your VM setup)");
    }

    if (params.get("p") != null) {
      myPort = Integer.parseInt(params.get("p").get(0));
    } else {
      printUsageWithError("Provide the port, that the middleware listens to (e.g. 11212)!");
    }

    if (params.get("m") != null) {
      mcAddresses = params.get("m");
    } else {
      printUsageWithError(
          "Give at least one memcached backend server IP address and port (e.g. 123.11.11.10:11211)!");
    }

    if (params.get("t") != null) {
      numThreadsPTP = Integer.parseInt(params.get("t").get(0));
    } else {
      printUsageWithError(
          "Provide the number of threads for the threadpool for each server (e.g. 4)!");
    }

    if (params.get("r") != null) {
      writeToCount = Integer.parseInt(params.get("r").get(0));
    } else {
      printUsageWithError("Provide the replication factor (1=not replicated)!");
    }

    if (params.get("v") != null) {
      int num = Integer.parseInt(params.get("v").get(0));
      switch (num) {
        case 0:
          verbosity = Level.OFF;
          break;
        case 1:
          verbosity = Level.INFO;
          break;
        case 2:
          verbosity = Level.FINE;
          break;
        case 3:
          verbosity = Level.FINER;
          break;
        case 4:
          verbosity = Level.FINEST;
          break;
        case 5:
          verbosity = Level.ALL;
          break;
      }
    }
    if (params.get("f") != null) {
      logfile = params.get("f").get(0);
    }
    if (params.get("R") != null) {
      readSampleRate = Integer.parseInt(params.get("R").get(0));
    }
    if (params.get("W") != null) {
      writeSampleRate = Integer.parseInt(params.get("W").get(0));
    }
  }

  private static void printUsageWithError(String errorMessage) {
    System.err.println();
    System.err.println(
        "Usage: -l <MyIP> -p <MyListenPort> -t <NumberOfThreadsInPools> -r <WriteToThisManyServers> -m <MemcachedIP:Port> <MemcachedIP2:Port2> ...");
    if (errorMessage != null) {
      System.err.println();
      System.err.println("Error message: " + errorMessage);
    }
    System.exit(1);
  }
}
