package com.dragonflow.siteview.websphere.util;

public class WebSphereServerCommandFactory
{
//  public static final Log logger = LogFactory.getEasyLog(WebSphereServerCommandFactory.class);

  private static WebSphereServerCommandFactory INSTANCE = null;

  public static synchronized WebSphereServerCommandFactory getInstance()
  {
    if (INSTANCE == null) {
      INSTANCE = new WebSphereServerCommandFactory();
    }
    return INSTANCE;
  }

  public IWebSphereServerCommand<String> getCounterListCommand() {
    return new WebSphereServerCounterListCommand();
  }

  public IWebSphereServerCommand<WebSphereCounter[]> getCounterValuesCommand(WebSphereCounter[] counters) {
    return new WebSphereServerCounterValuesCommand(counters);
  }

  public IWebSphereServerCommand<String> getServerNameCommand() {
    return new WebSphereServerNameCommand();
  }

  private class WebSphereServerNameCommand
    implements IWebSphereServerCommand<String>
  {
    public IWebSphereServerCommandResult<String> execute(IWebSphereServerConnector webSphereServerConnector)
      throws WebSphereServerCommandException
    {
      try
      {
        return getServerName(webSphereServerConnector.getWebSphereConnectionProperties());
      } catch (Exception e) {
        throw new WebSphereServerCommandException("An error occurred while attempting to get WebSphere server name", e);
      }
    }

    private IWebSphereServerCommandResult<String> getServerName(WebSphereConnectionProperties webSphereConnectionProperties) {
      return new WebSphereServerCommandResult(webSphereConnectionProperties.getServerName());
    }
  }

  private class WebSphereServerCounterValuesCommand
    implements IWebSphereServerCommand<WebSphereCounter[]>
  {
    WebSphereCounter[] counters;

    protected WebSphereServerCounterValuesCommand(WebSphereCounter[] paramArrayOfWebSphereCounter)
    {
      this.counters = paramArrayOfWebSphereCounter;
    }

    public IWebSphereServerCommandResult<WebSphereCounter[]> execute(IWebSphereServerConnector webSphereServerConnector)
      throws WebSphereServerCommandException
    {
      if (this.counters != null) {
        return execute(webSphereServerConnector, this.counters);
      }
      return null;
    }

    private IWebSphereServerCommandResult<WebSphereCounter[]> execute(IWebSphereServerConnector webSphereServerConnector, WebSphereCounter[] counters) throws WebSphereServerCommandException
    {
      try
      {
        return getCounterValues(webSphereServerConnector.getWsMonitoringConnector(), counters);
      } catch (Exception e) {
        throw new WebSphereServerCommandException("An error occurred while attempting to retrieve the counter server values", e);
      }
    }

    private IWebSphereServerCommandResult<WebSphereCounter[]> getCounterValues(WebSphereMonitorImpl websphereMonitorImpl, WebSphereCounter[] counters)
      throws Exception
    {
      return new WebSphereServerCommandResult(websphereMonitorImpl.getCounterValues(counters));
    }
  }

  private class WebSphereServerCounterListCommand
    implements IWebSphereServerCommand<String>
  {
    public IWebSphereServerCommandResult<String> execute(IWebSphereServerConnector webSphereServerConnector)
      throws WebSphereServerCommandException
    {
      try
      {
        return getCounterList(webSphereServerConnector.getWsMonitoringConnector());
      } catch (Exception e) {
        throw new WebSphereServerCommandException("An error occurred while attempting to retrieve list of WebSphere counters", e);
      }
    }

    private IWebSphereServerCommandResult<String> getCounterList(WebSphereMonitorImpl websphereMonitorImpl)
      throws Exception
    {
      StringBuffer xml = new StringBuffer();
      if (websphereMonitorImpl.getCounterList(xml)) {
        return new WebSphereServerCommandResult(xml.toString());
      }

      throw new Exception("WebSphereMonitor received exception in getCounterList(): " + xml.toString());
    }
  }
}