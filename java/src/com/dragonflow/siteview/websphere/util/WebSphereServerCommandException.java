package com.dragonflow.siteview.websphere.util;

public class WebSphereServerCommandException extends Exception
{
  public WebSphereServerCommandException()
  {
  }

  public WebSphereServerCommandException(String message)
  {
    super(message);
  }

  public WebSphereServerCommandException(String message, Throwable cause)
  {
    super(message, cause);
  }

  public WebSphereServerCommandException(Throwable cause)
  {
    super("A remote WebSphere Application Server command could not be executed due to an exception", cause);
  }
}