package com.dragonflow.siteview.websphere.util;

import java.rmi.Remote;
import java.rmi.RemoteException;

public abstract interface WebSphereServer extends Remote
{
  public abstract WebSphereCounter[] getCounters(WebSphereCounter[] paramArrayOfWebSphereCounter)
    throws RemoteException;

  public abstract String getBrowseData()
    throws RemoteException;

  public abstract String getServerName()
    throws RemoteException;
}