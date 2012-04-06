package com.dragonflow.siteview.websphere.util;

import java.io.File;
import java.io.PrintStream;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.soap.providers.com.Log;

public class WebSphereServiceImpl extends UnicastRemoteObject
  implements WebSphereService
{
//  private static String WEBSPHERE_LOG_FILE_PATH = Platform.getRoot() + File.separator + "logs" + File.separator + "websphere_monitors_" + Platform.currentDateToString() + ".log";

  protected Map servers = Collections.synchronizedMap(new HashMap());
  protected static final String packagePrefix = "com.dragonflow.siteview.websphere.util";
  protected String url = null;
  boolean debug = Boolean.getBoolean("websphere.monitor.debug");
  private long token;

  public WebSphereServiceImpl()
    throws RemoteException
  {
    this.url = System.getProperty("com.dragonflow.siteview.websphere.util.registryURL", "");
    try {
      this.token = Long.parseLong(System.getProperty("com.dragonflow.siteview.websphere.util.token"));
    } catch (NumberFormatException e) {
      this.token = -1L;
    }
  }

  public static void main(String[] args)
  {
	  
	  System.out.println("WebSphereServiceImpl installing new SecurityManager... ");
  // Platform.redirectStandardOutputAndError(WEBSPHERE_LOG_FILE_PATH);
    if (System.getSecurityManager() == null) {
      System.out.println("WebSphereServiceImpl installing new SecurityManager... ");
      System.setSecurityManager(new NullSecurityManager());
    }

    WebSphereServiceImpl wsService = null;
    try {
      wsService = new WebSphereServiceImpl();
      System.out.println("Attempting to bind WebSphereService in rmiregistry as " + wsService.url);
      Naming.rebind(wsService.url, wsService);
      System.out.println("WebSphereService successfully bound in rmiregistry as " + wsService.url);
      System.out.flush();
      long heartBeatFreq;
      try
      {
        heartBeatFreq = Long.parseLong(System.getProperty("com.dragonflow.siteview.websphere.util.heartBeatFrequency", "3000"));
      } catch (NumberFormatException e) {
        heartBeatFreq = 3000L;
        System.err.println("WebSphereService process could not parse the heartBeatFrequencyproperty.  Using default of " + heartBeatFreq + " milliseconds.");
      }

      String registryHost = System.getProperty("com.dragonflow.siteview.websphere.util.host", "localhost");
      int registryPort;
      try
      {
        registryPort = Integer.parseInt(System.getProperty("com.dragonflow.siteview.websphere.util.port", "1099"));
      } catch (NumberFormatException e) {
        registryPort = 1099;
        System.err.println("WebSphereService process could not parse the portproperty.  Using default of " + registryPort + " milliseconds.");
      }

      new RegistryHeartBeat(wsService.token, wsService.url, registryHost, registryPort, heartBeatFreq).start();
    }
    catch (Exception e) {
      System.err.println("WebSphereServiceImpl.main() encountered an exception: " + e);
      e.printStackTrace();
    }
  }

  public WebSphereServer getServer(WebSphereConnectionProperties connProps)
    throws RemoteException
  {
    if (this.debug) System.err.println("Entering WebSphereServiceImpl.getServer() with connProps=" + connProps);
    WebSphereServer server;
    synchronized (this) {
      server = (WebSphereServer)this.servers.get(connProps.getHashID());
      if (this.debug) System.err.println("WebSphereServiceImpl.getServer() got cached server=" + server + " with connProps.getHashID()=" + connProps.getHashID());

      if (server == null) {
        if (this.debug) System.err.println("WebSphereServiceImpl.getServer() is instantiating new server because none was found in the cache.");
        server = new WebSphereServerImpl(connProps);
        this.servers.put(connProps.getHashID(), server);
      }
    }

    if (this.debug) System.err.println("Leaving WebSphereServiceImpl.getServer() with server=" + server);
    return server;
  }

  public long getToken() throws RemoteException
  {
    return this.token;
  }
}
