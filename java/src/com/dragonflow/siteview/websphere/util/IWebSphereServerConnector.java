package com.dragonflow.siteview.websphere.util;


public abstract interface IWebSphereServerConnector
{
  public abstract WebSphereMonitorImpl getWsMonitoringConnector();

  public abstract WebSphereConnectionProperties getWebSphereConnectionProperties();
}