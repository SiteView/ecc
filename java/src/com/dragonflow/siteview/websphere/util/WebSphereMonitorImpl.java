package com.dragonflow.siteview.websphere.util;

import java.io.PrintStream;

public abstract class WebSphereMonitorImpl
{
  protected String username;
  protected String password;
  protected String port = "900";
  protected String host;
  protected WebSphereConnectionProperties connProps;
  boolean debug = Boolean.getBoolean("websphere.monitor.debug");

  public WebSphereMonitorImpl(WebSphereConnectionProperties connProps) {
    this.connProps = connProps;
    this.host = connProps.getServerName();
    this.port = Integer.toString(connProps.getPort());
    this.username = connProps.getUsername();
    this.password = connProps.getPassword();
  }

  protected void error(String msg, Exception e)
  {
    System.err.println(msg);
    if ((this.debug) && (e != null))
      e.printStackTrace();
  }

  protected void error(String msg) {
    error(msg, null);
  }

  public abstract void connect()
    throws ConnectionException;

  public abstract boolean getCounterList(StringBuffer paramStringBuffer);

  public abstract WebSphereCounter[] getCounterValues(WebSphereCounter[] paramArrayOfWebSphereCounter);
}