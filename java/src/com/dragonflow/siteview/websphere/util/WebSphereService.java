package com.dragonflow.siteview.websphere.util;

import java.rmi.RemoteException;

public abstract interface WebSphereService extends HeartBeatListener
{
  public static final String URL_PROPERTY = "registryURL";
  public static final String WEBSPHERE_MONITOR_PROPERTY = "websphere.monitor";
  public static final String DEBUG_PROPERTY = "websphere.monitor.debug";
  public static final String REMOTE_DEBUGGER_PROPERTY = "websphere.monitor.remote.debugger";
  public static final String REMOTE_DEBUGGER_PORT_PROPERTY = "websphere.monitor.remote.debugger.port";
  public static final String HEART_BEAT_FREQUENCY = "heartBeatFrequency";
  public static final String HOST = "host";
  public static final String PORT = "port";
  public static final String TOKEN_PROPERTY = "token";

  public abstract WebSphereServer getServer(WebSphereConnectionProperties paramWebSphereConnectionProperties)
    throws RemoteException;
}
