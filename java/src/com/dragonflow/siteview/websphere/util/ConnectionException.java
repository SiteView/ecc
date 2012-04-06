package com.dragonflow.siteview.websphere.util;

import java.rmi.RemoteException;

public class ConnectionException extends RemoteException
{
  Exception e;
  Throwable th;

  public ConnectionException(Throwable th)
  {
    super(th.getMessage(), th);
  }

  public ConnectionException(Exception e) {
    super("Inner exception is: " + e);
    this.e = e;
  }

  public ConnectionException(String msg) {
    super(msg);
  }

  public Exception getInnerException() {
    return this.e;
  }
}