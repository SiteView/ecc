package com.dragonflow.siteview.websphere.util;

public class WebSphereServerConnectorException extends Exception
{
  public WebSphereServerConnectorException()
  {
  }

  public WebSphereServerConnectorException(String message)
  {
    super(message);
  }

  public WebSphereServerConnectorException(String message, Throwable cause)
  {
    super(message, cause);
  }

  public WebSphereServerConnectorException(Throwable cause)
  {
    super("A connector to WebSphere Application Server could not be created due to an exception", cause);
  }
}