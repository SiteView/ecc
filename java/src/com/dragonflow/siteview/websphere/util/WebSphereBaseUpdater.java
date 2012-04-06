package com.dragonflow.siteview.websphere.util;

public final class WebSphereBaseUpdater<T> extends WebSphereAbstractUpdater
implements IWebSphereBaseUpdater
{
//public static final Log logger = LogFactory.getEasyLog(WebSphereBaseUpdater.class);
private IWebSphereServerCommand<T> serverCommand;
private IWebSphereServerCommandResult<T> commandResult;

public WebSphereBaseUpdater(WebSphereConnectionProperties webSphereConnectionProperties, ClassLoader webSphereClassLoader, IWebSphereServerCommand<T> serverCommand)
{
  super(webSphereConnectionProperties, webSphereClassLoader);
  this.serverCommand = serverCommand;
}

public WebSphereBaseUpdater(WebSphereConnectionProperties webSphereConnectionProperties, IWebSphereServerCommand<T> serverCommand)
{
  this(webSphereConnectionProperties, null, serverCommand);
}

protected void getDataFromWebSphereServer(IWebSphereServerConnector webSphereServerConnector)
  throws WebSphereServerGetDataException
{
//  if (logger.isDebugEnabled()) {
//    logger.debug("Entering getDataFromWebSphereServer()");
//  }

  if (this.serverCommand == null) {
    throw new WebSphereServerGetDataException(new NullPointerException("WebSphere server 'command object' was expected to be non-null, but was null: serverCommand=" + this.serverCommand));
  }
  try
  {
    IWebSphereServerCommandResult result = executeWebSphereServerCommand(webSphereServerConnector, this.serverCommand);
    setCommandResult(result);
  } catch (WebSphereServerCommandException e) {
    throw new WebSphereServerGetDataException("SiteView was unable to get data from WebSphere Application Server due to an exception", e);
  }

//  if (logger.isDebugEnabled())
//    logger.debug("Leaving getDataFromWebSphereServer()");
}

private IWebSphereServerCommandResult<T> executeWebSphereServerCommand(IWebSphereServerConnector webSphereServerConnector, IWebSphereServerCommand<T> serverCommand)
  throws WebSphereServerCommandException
{
//  if (logger.isDebugEnabled()) {
//    logger.debug("Entering executeWebSphereServerCommand()");
//  }

  IWebSphereServerCommandResult result = serverCommand.execute(webSphereServerConnector);

//  if (logger.isDebugEnabled()) {
//    logger.debug("Leaving executeWebSphereServerCommand()");
//  }

  return result;
}

public IWebSphereServerCommand<T> getServerCommand()
{
  return this.serverCommand;
}

public void setServerCommand(IWebSphereServerCommand<T> serverCommand)
{
  this.serverCommand = serverCommand;
}

public IWebSphereServerCommandResult<T> getCommandResult()
{
  return this.commandResult;
}

public void setCommandResult(IWebSphereServerCommandResult<T> commandResult)
{
  this.commandResult = commandResult;
}
}
