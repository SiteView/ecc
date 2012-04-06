package com.dragonflow.siteview.websphere.util;

public abstract interface IWebSphereServerCommandResult<T>
{
  public abstract void setResult(T paramT);

  public abstract T getResult();
}