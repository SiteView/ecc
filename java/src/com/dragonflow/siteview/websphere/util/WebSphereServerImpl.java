package com.dragonflow.siteview.websphere.util;

import java.io.PrintStream;
import java.lang.reflect.Constructor;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class WebSphereServerImpl extends UnicastRemoteObject
  implements WebSphereServer
{
  private WebSphereConnectionProperties connProps;
  private static String PACKAGE_NAME = "com.dragonflow.siteview.websphere.util";
  private WebSphereMonitorImpl wsMonitoring;

  public WebSphereServerImpl(WebSphereConnectionProperties cp)
    throws RemoteException
  {
    this.connProps = cp;
    String wsMonitorImplClassName = PACKAGE_NAME + ".WebSphereMonitor" + this.connProps.getAPI();

    if (this.wsMonitoring != null) return;
    try {
      this.wsMonitoring = ((WebSphereMonitorImpl)Class.forName(wsMonitorImplClassName).getConstructor(new Class[] { WebSphereConnectionProperties.class }).newInstance(new Object[] { this.connProps }));
    }
    catch (Exception e)
    {
      System.err.println("Exception occurred while instantiating " + wsMonitorImplClassName + ": " + e.getMessage());
      System.err.println("Exception toString(): " + e.toString());
      e.printStackTrace(System.err);
      if (e.getCause() != null) {
        throw new RemoteException("A remote WebSphereServer could not be created due to an exception: " + e.getCause());
      }
      throw new RemoteException("A remote WebSphereServer could not be created due to an exception: " + e);
    }
  }

  public String getBrowseData()
    throws RemoteException
  {
    StringBuffer xml = new StringBuffer();
    if (this.wsMonitoring.getCounterList(xml)) {
      return xml.toString();
    }

    RemoteException re = new RemoteException(xml.toString());
    throw re;
  }

  public WebSphereCounter[] getCounters(WebSphereCounter[] counters)
    throws RemoteException
  {
    return this.wsMonitoring.getCounterValues(counters);
  }

  public String getServerName() throws RemoteException {
    return this.connProps.getServerName();
  }
}