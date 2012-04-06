package com.dragonflow.siteview.websphere.util;

import org.apache.commons.lang.exception.ExceptionUtils;
public abstract class WebSphereAbstractUpdater
implements Runnable
{
//public static final Log logger = LogFactory.getEasyLog(WebSphereAbstractUpdater.class);
private ClassLoader webSphereClassLoader;
private WebSphereConnectionProperties webSphereConnectionProperties;
private IWebSphereServerConnector webSphereServerConnector;
private Throwable throwable;
private StringBuilder errors;
private boolean isUpdateSuccessful;

private WebSphereAbstractUpdater()
{
  this.webSphereServerConnector = null;

  this.throwable = null;
  this.errors = new StringBuilder();
  this.isUpdateSuccessful = true;
}

public WebSphereAbstractUpdater(WebSphereConnectionProperties webSphereConnectionProperties, ClassLoader webSphereClassLoader)
{
  this();
  this.webSphereConnectionProperties = webSphereConnectionProperties;
  this.webSphereClassLoader = webSphereClassLoader;
}

public WebSphereAbstractUpdater(WebSphereConnectionProperties webSphereConnectionProperties)
{
  this(webSphereConnectionProperties, null);
}

public final void run()
{
  try
  {
    execute();
  }
  catch (Throwable throwable) {
    setUpdateSuccessful(false);
//    logger.error("Unexpected exception during thread execution", throwable);
    this.errors.append("Unexpected exception occurred: " + ExceptionUtils.getRootCause(throwable).getMessage());
    setThrowable(throwable);
  }
}

private void execute()
  throws Throwable
{
//  if (logger.isDebugEnabled()) {
//    logger.debug("Entering WebSphereAbstractUpdater().run()");
//  }
  try
  {
    setWebSphereClassLoader();
    initWebSphereServerConnector();
  } catch (SecurityException e) {
    setUpdateSuccessful(false);
//    logger.error("Security manager has indicated a security violation");
    this.errors.append("Failed to set the proper thread class loader. Security manager has indicated a security violation (See the logs for details)");

    setThrowable(e);
  } catch (WebSphereServerConnectorException e) {
    setUpdateSuccessful(false);
//    logger.error("Could not connect to WebSphere Application Server. A connector to WebSphere Application Server could not be created due to an exception. WebSphere Application Server connector was expected to be non-null, but was null: webSphereServerConnector=" + this.webSphereServerConnector);

    this.errors.append("Could not make a connection to WebSphere Application Server. Please check the connection properties, verify that the application server is running and try again (See the logs for details)");

    setThrowable(e);
  }

  if (this.webSphereServerConnector != null) {
    try
    {
      getDataFromWebSphereServer(this.webSphereServerConnector);
    } catch (WebSphereServerGetDataException e) {
      setUpdateSuccessful(false);
      String msg = "Connection successfully established to WebSphere Application Server but SiteView was unable to get data from it (See the logs for details)";

//      logger.error(msg, e);
      this.errors.append(msg);
      setThrowable(e);
    }

  }

  if ((this.isUpdateSuccessful) && ((
    (this.errors.length() != 0) || (this.throwable != null)))) {
    setUpdateSuccessful(false);
  }

//  if (logger.isDebugEnabled())
//    logger.debug("Leaving WebSphereAbstractUpdater().run()");
}

private void setWebSphereClassLoader()
  throws SecurityException
{
  if (this.webSphereClassLoader != null)
    Thread.currentThread().setContextClassLoader(this.webSphereClassLoader);
}

private void initWebSphereServerConnector()
  throws WebSphereServerConnectorException
{
  this.webSphereServerConnector = new WebSphereServerConnector(this.webSphereConnectionProperties);
}

protected abstract void getDataFromWebSphereServer(IWebSphereServerConnector paramIWebSphereServerConnector)
  throws WebSphereServerGetDataException;

public IWebSphereServerConnector getWebSphereServerConnector()
{
  return this.webSphereServerConnector;
}

public WebSphereConnectionProperties getWebSphereConnectionProperties()
{
  return this.webSphereConnectionProperties;
}

public ClassLoader getWebSphereClassLoader()
{
  return this.webSphereClassLoader;
}

public void setThrowable(Throwable throwable)
{
  this.throwable = throwable;
}

public Throwable getThrowable()
{
  return this.throwable;
}

public StringBuilder getErrors()
{
  return this.errors;
}

public void setErrors(StringBuilder errors)
{
  this.errors = errors;
}

public boolean isUpdateSuccessful()
{
  return this.isUpdateSuccessful;
}

public void setUpdateSuccessful(boolean isUpdateSuccessful)
{
  this.isUpdateSuccessful = isUpdateSuccessful;
}
}
