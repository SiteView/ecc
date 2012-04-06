package com.dragonflow.siteview.websphere.util;

import java.lang.reflect.Constructor;

public class WebSphereServerConnector
  implements IWebSphereServerConnector
{
//  private static final Log logger = LogFactory.getEasyLog(WebSphereServerConnector.class);
  private WebSphereConnectionProperties webSphereConnectionProperties;
  private static String PACKAGE_NAME = "com.dragonflow.siteview.websphere.util";
  private WebSphereMonitorImpl wsMonitoringConnector;

  public WebSphereServerConnector(WebSphereConnectionProperties webSphereConnectionProperties)
    throws WebSphereServerConnectorException
  {
    this.webSphereConnectionProperties = webSphereConnectionProperties;
    initMonitorImplInstance();
  }

  private void initMonitorImplInstance()
    throws WebSphereServerConnectorException
  {
    String wsMonitorImplClassName = PACKAGE_NAME + ".WebSphereMonitor" + getWebSphereConnectionProperties().getAPI();
    if (getWsMonitoringConnector() != null) return;
    try {
      this.wsMonitoringConnector = ((WebSphereMonitorImpl)Class.forName(wsMonitorImplClassName).getConstructor(new Class[] { WebSphereConnectionProperties.class }).newInstance(new Object[] { getWebSphereConnectionProperties() }));
    }
    catch (Exception e)
    {
      throw new WebSphereServerConnectorException("Exception occurred while instantiating " + wsMonitorImplClassName, e);
    }
  }

  public WebSphereConnectionProperties getWebSphereConnectionProperties()
  {
    return this.webSphereConnectionProperties;
  }

  public WebSphereMonitorImpl getWsMonitoringConnector()
  {
    return this.wsMonitoringConnector;
  }
}