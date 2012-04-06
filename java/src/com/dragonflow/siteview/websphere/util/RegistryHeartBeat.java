package com.dragonflow.siteview.websphere.util;

import java.io.PrintStream;
import java.rmi.Naming;

public class RegistryHeartBeat extends Thread
{
  int registryPort = 1099;
  String registryHost = "127.0.0.1";
  long frequency = 2000L;
  String url;
  long token;

  public RegistryHeartBeat(long token, String url, String host, int port, long freq)
  {
    this.token = token;
    this.url = url;
    this.registryHost = host;
    this.registryPort = 1099;
    this.frequency = 2000L;
  }

  public void run() {
    while (true) {
      HeartBeatListener hbListener = null;
      try {
        hbListener = (HeartBeatListener)Naming.lookup(this.url);
        if (hbListener.getToken() != this.token)
          System.exit(0);
      }
      catch (Exception e) {
        e.printStackTrace();
        System.exit(0);
      }
      try
      {
        Thread.sleep(this.frequency);
      } catch (InterruptedException e) {
        System.err.println("Sleep interrupted in HeartBeat Thread in WebSphereService process.");
      }
    }
  }
}

