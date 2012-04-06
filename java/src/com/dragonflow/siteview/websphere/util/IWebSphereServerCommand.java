package com.dragonflow.siteview.websphere.util;


public abstract interface IWebSphereServerCommand<T>
{
  public abstract IWebSphereServerCommandResult<T> execute(IWebSphereServerConnector paramIWebSphereServerConnector)
    throws WebSphereServerCommandException;
}