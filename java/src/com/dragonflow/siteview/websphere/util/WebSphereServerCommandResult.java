package com.dragonflow.siteview.websphere.util;

public class WebSphereServerCommandResult<T>
implements IWebSphereServerCommandResult<T>
{
private T result;

public WebSphereServerCommandResult()
{
}

public WebSphereServerCommandResult(T result)
{
  setResult(result);
}

public void setResult(T result)
{
  this.result = result;
}

public T getResult()
{
  return this.result;
}
}