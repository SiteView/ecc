package com.dragonflow.siteview.websphere.util;

public class ClasspathResourcesException extends Exception
{
  private static final long serialVersionUID = 1L;

  public ClasspathResourcesException()
  {
  }

  public ClasspathResourcesException(String message)
  {
    super(message);
  }

  public ClasspathResourcesException(Throwable cause)
  {
    super(cause);
  }

  public ClasspathResourcesException(String message, Throwable cause)
  {
    super(message, cause);
  }
}