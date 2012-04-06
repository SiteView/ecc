package com.dragonflow.siteview.websphere.util;

public class WebSphereServerGetDataException extends Exception
{
  public WebSphereServerGetDataException()
  {
  }

  public WebSphereServerGetDataException(String message)
  {
    super(message);
  }

  public WebSphereServerGetDataException(String message, Throwable cause)
  {
    super(message, cause);
  }

  public WebSphereServerGetDataException(Throwable cause)
  {
    super("SiteView was unable to get data from WebSphere Application Server due to an exception", cause);
  }
}