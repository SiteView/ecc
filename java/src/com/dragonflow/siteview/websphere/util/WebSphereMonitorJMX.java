package com.dragonflow.siteview.websphere.util;

import java.io.CharArrayWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;

import com.dragonflow.siteview.infra.util.ServicePlatform;

public class WebSphereMonitorJMX extends WebSphereMonitorImpl
{
  WebSphereJMXInterface management = null;
  Object adminClient = null;
  Class adminClientClass = null;
  Hashtable addedMBeans = null;

  Object[] statConfigs = null;

  HashMap configMap = new HashMap();

  Object perfOName = null;

  Object serverOName = null;
  private String trustStore;
  private String trustStorePassword;
  private String keyStore;
  private String keyStorePassword;
  Set objectNameSet;
  HashSet filters = new HashSet();

  private static Properties props = new Properties();
  static String propsFilename = ServicePlatform.getRoot()
	+ "/templates.applications/websphere.props";
  private static final String WEBSPHERE_MBEANS = "WebSphere:*";
  private static final String REAL_SLASH_DELIM = "SIS_REAL_SLASH";
 private static final String msIbmSpecificWebSphereDataPrefix = "was:/server/";
  private static final int msIbmSpecificWebSphereDataPrefixLength = "was:/server/".length();

  public WebSphereMonitorJMX(WebSphereConnectionProperties connProps)
    throws ConnectionException
  {
    super(connProps);

    this.trustStore = connProps.getTrustStore();
    this.trustStorePassword = connProps.getTrustStorePassword();
    this.keyStore = connProps.getKeyStore();
    this.keyStorePassword = connProps.getKeyStorePassword();

    if (this.debug) System.err.println("Entering WebSphereMonitorJMX() constructor.");
    connect();
    if (!(this.debug)) return; System.err.println("Leaving WebSphereMonitorJMX() constructor.");
  }

  public void connect()
    throws ConnectionException
  {
    if (this.debug) System.err.println("Entering WebSphereMonitorJMX.connect().");

    try
    {
      for (int i = 1; ; ++i) {
        String filter = props.getProperty("jmxFilter" + i);
        if (filter == null) {
          break;
        }
        this.filters.add(filter);
      }

      Properties clientProps = new Properties();
      clientProps.setProperty("type", props.getProperty("jmxProtocol"));
      clientProps.setProperty("host", this.host);
      clientProps.setProperty("port", this.port);
      if ((this.username == null) && (this.password == null))
        clientProps.setProperty("securityEnabled", "false");
      else {
        clientProps.setProperty("securityEnabled", "true");
      }
      if (this.username != null) {
        clientProps.setProperty("username", this.username);
      }
      if (this.password != null) {
        clientProps.setProperty("password", this.password);
      }

      if ((this.trustStore != null) && (!(this.trustStore.equals(""))) && (this.keyStore != null) && (!(this.keyStore.equals(""))) && (this.trustStorePassword != null) && (!(this.trustStorePassword.equals(""))) && (this.keyStorePassword != null) && (!(this.keyStorePassword.equals(""))))
      {
        clientProps.setProperty("javax.net.ssl.trustStore", this.trustStore);
        clientProps.setProperty("javax.net.ssl.keyStore", this.keyStore);
        clientProps.setProperty("javax.net.ssl.trustStorePassword", this.trustStorePassword);
        clientProps.setProperty("javax.net.ssl.keyStorePassword", this.keyStorePassword);
      }

      Class adminClientFactoryClass = Thread.currentThread().getContextClassLoader().loadClass("com.ibm.websphere.management.AdminClientFactory");
      Method acfCreateAdminClientMethod = adminClientFactoryClass.getMethod("createAdminClient", new Class[] { Properties.class });

      if (this.debug) System.err.println("WebSphereMonitorJMX.connect() creating adminClient with clientProps.host=" + clientProps.getProperty("host") + " clientProps.port=" + clientProps.getProperty("port") + " clientProps.securityEnabled=" + clientProps.getProperty("securityEnabled") + ".");

      this.adminClient = acfCreateAdminClientMethod.invoke(null, new Object[] { clientProps });
      if (this.debug) System.err.println("WebSphereMonitorJMX.connect() created adminClient=" + this.adminClient + ".");
      this.adminClientClass = this.adminClient.getClass();
      this.management = new WebSphereJMXInterface(this.adminClientClass, this.connProps);

      Object on = this.management.objectNameCtr.newInstance(new Object[] { "WebSphere:*" });
      if (this.debug) System.err.println("WebSphereMonitorJMX.connect() invoking AdminClient.queryNames() with ObjectName on=WebSphere:*.");
      this.objectNameSet = ((Set)this.management.queryNamesMethod.invoke(this.adminClient, new Object[] { on, null }));
      findONames();
    }
    catch (Exception e) {
      error("Exception in WebSphereMonitorJMX.connect(): " + e.toString(), e);

      throw new ConnectionException(e);
    } catch (Throwable t) {
      t.printStackTrace();
    } finally {
      if (this.debug) System.err.println("Leaving WebSphereMonitorJMX.connect().");
    }
  }

  public boolean getCounterList(StringBuffer xml)
  {
    if (this.debug) System.err.println("Entering WebSphereMonitorJMX.getCounterList()."); HashMap descriptionMap ;
    try {
      this.addedMBeans = new Hashtable();
      WebSphereJMXObject root = new WebSphereJMXObject();

      descriptionMap = new HashMap();
      Object[] configs = getStatConfigs(this.perfOName);
      if (configs == null) {
        xml.append("Failed to get StatConfigs from perfOName=" + this.perfOName + ".  Possible cause is that the WebSphere client jars in the selected WebSphere directory do not match the version of the WebSphere server to be monitored.");
        return false;
      }
      for (int j = 0; j < configs.length; ++j) {
        String mbeanName = (String)this.management.getShortNameMethod.invoke(configs[j], null);
        this.configMap.put(mbeanName, configs[j]);

        Object[] dataInfos = (Object[])(Object[])this.management.listAllDataMethod.invoke(configs[j], null);
        if (dataInfos.length > 0) {
          for (int k = 0; k < dataInfos.length; ++k) {
            String comment = (String)this.management.getDICommentMethod.invoke(dataInfos[k], null);
            String diName = (String)this.management.getDINameMethod.invoke(dataInfos[k], null);
            descriptionMap.put(diName, comment);
          }
        }

      }

      Object stats = collectStats();
      WebSphereJMXObject obj = new WebSphereJMXObject(this.perfOName, this.adminClient, this.management, this.filters, stats, descriptionMap, "was:", "was:");
      addPerfMBeanObject(obj, root, this.perfOName);

      obj.setParent(root);
      Object on;
      if (this.objectNameSet != null) {
        on = null;
        for (Iterator it = this.objectNameSet.iterator(); it.hasNext(); ) {
          on = it.next();
          if (this.addedMBeans.containsKey(on)) {
            continue;
          }
          obj = new WebSphereJMXObject(on, this.adminClient, this.management, this.filters, this.configMap, this.perfOName);
          addObject(obj, root, this.perfOName);
        }
      }

      root.purge();
      CharArrayWriter writer = new CharArrayWriter();
      root.printXML(new PrintWriter(writer), 0);

      xml.append(writer.toString());


      return true;
    }
    catch (Exception e)
    {
      xml.append("Error collecting counters: " + e);
      error("Error collecting counters: " + e, e);
      descriptionMap=null;

      return false;
    }
    finally
    {
      if (this.debug) System.err.println("Leaving WebSphereMonitorJMX.getCounterList().");
    }
  }

  private WebSphereJMXObject findFiller(String name, String type, WebSphereJMXObject parent, Object perfOName)
    throws InstantiationException, IllegalAccessException, InvocationTargetException
  {
    if (this.debug) System.err.println("Entering WebSphereMonitorJMX.findFiller() with name=" + name + " type=" + type + "parent=" + parent + " and perfOName=" + perfOName + ".");

    WebSphereJMXObject retval = (WebSphereJMXObject)this.addedMBeans.get(name);
    if (retval == null) {
      if (this.debug) System.err.println("WebSphereMonitorJMX.findFiller() did not find name=" + name + " in addMBeans Map, creating new WebSphereJMXObject.");
      retval = new WebSphereJMXObject(this.management.objectNameCtr.newInstance(new Object[] { "WebSphere:name=" + name + ",type=" + type }), parent, this.adminClient, this.management, this.filters, this.configMap, perfOName);

      this.addedMBeans.put(name, retval);
    }
    if (this.debug) System.err.println("Leaving WebSphereMonitorJMX.findFiller() with retval=" + retval + ".");
    return retval;
  }

  private void addObject(WebSphereJMXObject obj, WebSphereJMXObject root, Object perfOName)
  {
    if (this.debug) System.err.println("Entering WebSphereMonitorJMX.addObject() with obj=" + obj + " root=" + root + "perfOName=" + perfOName + ".");
    WebSphereJMXObject parentObject = null;
    try
    {
      WebSphereJMXObject cellObj = findFiller(obj.getCell(), "cell", root, perfOName);
      WebSphereJMXObject nodeObj = findFiller(obj.getNode(), "node", cellObj, perfOName);
      WebSphereJMXObject processObj = findFiller(obj.getProcess(), "server", nodeObj, perfOName);

      WebSphereJMXObject additionalObj = findFiller("Additional Metrics", "additional", processObj, perfOName);

      String type = obj.getType();
      if (type != null) {
        WebSphereJMXObject typeObj = findFiller(type, type, additionalObj, perfOName);
        if (typeObj != null) {
          parentObject = typeObj;
          if (this.debug) System.err.println("WebSphereMonitorJMX.addObject() found parentObject=" + parentObject + ".");
        }
      }

      if (parentObject == null) {
        parentObject = additionalObj;
        if (this.debug) System.err.println("WebSphereMonitorJMX.addObject() found parentObject=" + parentObject + ".");
      }
    } catch (Exception e) {
      if (this.debug) System.err.println("Exception in WebSphereMonitorJMX.addObject(): " + e + ", setting parentObject to root=" + root + ".");
      parentObject = root;
    }

    obj.setParent(parentObject);
    if (this.debug) System.err.println("WebSphereMonitorJMX.addObject() invoking addSubObject on parentObject.");
    parentObject.addSubObject(obj);

    if (this.debug) System.err.println("WebSphereMonitorJMX.addObject() adding ObjectName=" + obj.getObjectName() + " to addedMBeans.");
    this.addedMBeans.put(obj.getObjectName(), obj);
    if (!(this.debug)) return; System.err.println("Leaving WebSphereMonitorJMX.addObject().");
  }

  private void addPerfMBeanObject(WebSphereJMXObject perfObj, WebSphereJMXObject root, Object perfOName)
  {
    if ((perfObj == null) || (perfObj.children == null)) {
      return;
    }
    WebSphereJMXObject cellObj = null;
    WebSphereJMXObject nodeObj = null;
    WebSphereJMXObject processObj = null;
    try {
      cellObj = findFiller(perfObj.getCell(), "cell", root, perfOName);
      nodeObj = findFiller(perfObj.getNode(), "node", cellObj, perfOName);
      processObj = findFiller(perfObj.getProcess(), "server", nodeObj, perfOName);
    } catch (Exception e) {
      error("Exception in WebSphereMonitorJMX.addPerfMBeanObject(): " + e, e);
      return;
    }

    for (Iterator it = perfObj.children.iterator(); it.hasNext(); ) {
      WebSphereJMXObject child = (WebSphereJMXObject)it.next();
      child.setParent(processObj);
      processObj.addSubObject(child);
    }
    this.addedMBeans.put(perfObj.getObjectName(), perfObj);
  }

  protected void getStatData(WebSphereCounter counter, Object counterStat)
    throws IllegalAccessException, InvocationTargetException
  {
    if (this.debug) System.err.println("Entering WebSphereMonitorJMX.getStatData() with counterStat=" + counterStat + ".");
    String classType = counterStat.getClass().getName();

    if ((classType.indexOf("com.ibm.websphere.pmi.stat.BoundaryStatisticImpl") != -1) || (classType.indexOf("com\\.ibm.ws.pmi.stat.BoundaryStatisticImpl") != -1))
    {
      counter.setValue(String.valueOf(this.management.getUpperBoundMethod.invoke(counterStat, null)));
    } else if ((classType.indexOf("com.ibm.websphere.pmi.stat.BoundedRangeStatisticImpl") != -1) || (classType.indexOf("com.ibm.ws.pmi.stat.BoundedRangeStatisticImpl") != -1))
    {
      counter.setValue(String.valueOf(this.management.getCurrentMethod.invoke(counterStat, null)));
    } else if ((classType.indexOf("com.ibm.websphere.pmi.stat.CountStatisticImpl") != -1) || (classType.indexOf("com.ibm.ws.pmi.stat.CountStatisticImpl") != -1))
    {
      counter.setValue(String.valueOf(this.management.getCountMethod.invoke(counterStat, null)));
    } else if ((classType.indexOf("com.ibm.websphere.pmi.stat.DoubleStatisticImpl") != -1) || (classType.indexOf("com.ibm.ws.pmi.stat.DoubleStatisticImpl") != -1))
    {
      counter.setValue(String.valueOf(this.management.getDoubleMethod.invoke(counterStat, null)));
    } else if ((classType.indexOf("com.ibm.websphere.pmi.stat.RangeStatisticImpl") != -1) || (classType.indexOf("com.ibm.ws.pmi.stat.RangeStatisticImpl") != -1))
    {
      counter.setValue(String.valueOf(this.management.getRSCurrentMethod.invoke(counterStat, null)));
    } else if ((classType.indexOf("com.ibm.websphere.pmi.stat.AverageStatisticImpl") != -1) || (classType.indexOf("com.ibm.ws.pmi.stat.AverageStatisticImpl") != -1))
    {
      counter.setValue(String.valueOf(this.management.getAverageMethod.invoke(counterStat, null)));
    } else if ((classType.indexOf("com.ibm.websphere.pmi.stat.TimeStatisticImpl") != -1) || (classType.indexOf("com.ibm.ws.pmi.stat.TimeStatisticImpl") != -1))
    {
      counter.setValue(getAverageTime(counterStat));
   } else {
     counter.setValue("n/a");
      counter.setError(true);
      counter.setErrorMessage("Error: Unrecognized counter type.");
    }
    if (!(this.debug)) return; System.err.println("Leaving WebSphereMonitorJMX.getStatData() with counter value: " + counter.getValue() + ".");
  }

  protected String getAverageTime(Object counterStat)
    throws IllegalAccessException, InvocationTargetException
  {
    double average = 0.0D;
    try
    {
      long count = ((Long)this.management.getTimeCountMethod.invoke(counterStat, null)).intValue();
      long totalTime = ((Long)this.management.getTotalMethod.invoke(counterStat, null)).intValue();
      if (count != 0L) {
        average = totalTime / count;
      }
      else
        average = totalTime;
    }
    catch (NumberFormatException ex)
    {
      return String.valueOf(this.management.getTotalMethod.invoke(counterStat, null));
    }
    return Double.toString(average);
  }

  protected void getIBMWebSphereValue(WebSphereCounter ctr, Object data)
  {
    String ctr_that_saves_slashes = ctr.getName();

    String counter = WebSphereCounter.denormalizeHierarchySlash(ctr_that_saves_slashes);

    if (this.debug) System.err.println("Entering WebSphereMonitorJXM.getIBMWebSphereValue2() with counter=" + counter + ".");

    String toMatch = counter.substring(msIbmSpecificWebSphereDataPrefixLength);

    Object currentStatsObject = data;
    try {
      StringTokenizer names = new StringTokenizer(toMatch, "/");

      int nTokens = names.countTokens();
      for (int i = 0; i < nTokens; ++i)
      {
        String toMatchName = names.nextToken();

        if ((toMatchName == null) || (toMatchName.length() <= 0)) {
          continue;
        }
       toMatchName = WebSphereCounter.replace(toMatchName, "ORIGINAL_NAME_SLASH", "/");

        Object subStat = this.management.getStatsMethod.invoke(currentStatsObject, new Object[] { toMatchName });
        if (subStat != null) {
          currentStatsObject = subStat;
        } else {
          Object statisticsObject = this.management.getStatisticMethod.invoke(currentStatsObject, new Object[] { toMatchName });
          if (statisticsObject != null) {
            getStatData(ctr, statisticsObject);
            break; }
          ctr.setError(true);
          ctr.setValue("n/a");
          ctr.setErrorMessage("Error: Unrecognized counter.");

          break;
        }
      }
    }
    catch (Exception e) {
      error("Exception in WebSphereMonitorJMX.getIBMWebSphereValue(): " + e.getMessage(), e);
      ctr.setError(true);
      ctr.setErrorMessage("Error: Failed to get value due to an exception: " + e.toString());
    }

    if (!(this.debug)) return; System.err.println("Leaving WebSphereMonitorJMX.getIBMWebSphereValue() with counter value=" + ctr.getValue() + ".");
  }

  public WebSphereCounter[] getCounterValues(WebSphereCounter[] counters)
  {
    if (this.debug) System.err.println("Entering WebSphereMonitorJMX.getCounterValues() with counters=" + counters);
    Object data = null;

    for (int i = 0; i < counters.length; ++i) {
      String object = null;
      String counter = WebSphereCounter.denormalize(counters[i].getName());

     if (counter.indexOf("was:/server/") == 0) {
        if (data == null) {
          data = collectStats();
          if (data == null)
          {
            return null;
          }
        }

        getIBMWebSphereValue(counters[i], data);
      } else {
        object = counters[i].getObjectNameWithMBeanIdentifier();
        counter = counters[i].getAttributeName();
        try
        {
          Object objectName = this.management.objectNameCtr.newInstance(new Object[] { object });
          Object info = this.management.getMBeanInfoMethod.invoke(this.adminClient, new Object[] { objectName });
          Object[] attrs = (Object[])(Object[])this.management.getAttributesMethod.invoke(info, null);
          boolean found = false;
          if (attrs == null) {
            error("No attrs got by getAttributes()");
            attrs = new Object[0];
          }
          for (int j = 0; j < attrs.length; ++j) {
            if ((attrs[j] == null) || 
              (!(((Boolean)this.management.isReadableMethod.invoke(attrs[j], null)).booleanValue()))) continue;
            String cName = (String)this.management.getNameMethod.invoke(attrs[j], null);
            if (counter.equals(cName)) {
              found = true;
              break;
            }

          }

          if (!(found))
          {
            Object stats = null;
            try {
              stats = this.management.getAttributeMethod.invoke(this.adminClient, new Object[] { objectName, "stats" });
            }
            catch (InvocationTargetException e) {
              if (this.debug) System.err.println("Exception occurred in WebSphereMonitorJMX.getCounterValues(): " + e);
            }
            if (stats != null) {
              Object[] configs = getStatConfigs(this.perfOName);
              String type = (String)this.management.getKeyPropertyMethod.invoke(objectName, new Object[] { "type" });

              if (configs == null) {
                configs = new Object[0];
              }

              for (int j = 0; j < configs.length; ++j) {
                if (configs[j] == null)
                  continue;
                String mbeanType = (String)this.management.getMBeanTypeMethod.invoke(configs[j], null);
                if (!(mbeanType.equals(type)))
                  continue;
                this.management.setConfigMethod.invoke(stats, new Object[] { configs[j] });
                Object counterStat = this.management.getStatisticMethod.invoke(stats, new Object[] { counter });
                if (counterStat != null) {
                  getStatData(counters[i], counterStat);
                  break; }
                counters[i].setValue("n/a");
                counters[i].setError(true);
                counters[i].setErrorMessage("Error: Unrecognized counter (could not find it in 'stats' attribute of MBean).");

                break;
              }
            }
          }
          else
          {
            String val = String.valueOf(this.management.getAttributeMethod.invoke(this.adminClient, new Object[] { objectName, counter }));
            if (val == null) {
              counters[i].setValue("n/a");
              counters[i].setError(true);
              counters[i].setErrorMessage("Error: Counter value was 'null'.");
            } else {
              counters[i].setValue(val);
            }
          }
        } catch (InvocationTargetException ite) {
          error("Exception in WebSphereMonitorJMX.getCounterValues: " + ite, ite);
          
          //Âß¼­¸Ä±äÁË
          counters[i].setValue("n/a");
         // return null;
        } catch (Exception e) {
          counters[i].setErrorMessage("Error: Failed to get value due to an exception: " + e.toString());
          counters[i].setError(true);
          counters[i].setValue("n/a");
          error("Exception in WebSphereMonitorJMX.getCounterValues: " + e, e);
        }
      }
    }
    if (this.debug) System.err.println("Leaving WebSphereMonitorJMX.getCounterValues() and returning counters=" + counters + ".");
    return counters;
  }

  protected void findONames()
    throws ConnectionException
  {
    String serverName = findPerfMBean();
    findServerMBean(serverName);
  }

  protected String findPerfMBean() throws ConnectionException {
    String serverName = "";
    Object on;
    Iterator it;
    try {
      on = null;
      for (it = this.objectNameSet.iterator(); it.hasNext(); ) {
        on = it.next();
        String type = (String)this.management.getKeyPropertyMethod.invoke(on, new Object[] { "type" });
        if ((type != null) && (type.equalsIgnoreCase("perf"))) {
          this.perfOName = on;
          serverName = (String)this.management.getKeyPropertyMethod.invoke(on, new Object[] { "process" });
          return serverName;
        }
      }
    } catch (Exception e) {
      error("Exception in WebSphereMonitorJMX.findPerfMBean(): " + e, e);
    }

    if (this.perfOName == null) {
      throw new ConnectionException("Could not find PerfMBean on WebSphere Application Server.  Is the Performance Monitoring Service enabled?");
    }

    return serverName;
  }

  protected void findServerMBean(String serverName)
    throws ConnectionException
  {
    Object on;
    Iterator it;
    try
    {
      on = null;
      for (it = this.objectNameSet.iterator(); it.hasNext(); ) {
        on = it.next();
        String type = (String)this.management.getKeyPropertyMethod.invoke(on, new Object[] { "type" });
        if ((type != null) && (type.equalsIgnoreCase("server"))) {
          String thisServer = (String)this.management.getKeyPropertyMethod.invoke(on, new Object[] { "name" });
          if ((thisServer != null) && (serverName.equals(thisServer))) {
            this.serverOName = on;
            break;
          }
        }
      }
    } catch (Exception e) {
      System.out.println("Exception in WebSphereMonitorJMX.findServerMBean(): " + e);
    }

    if (this.serverOName == null)
      throw new ConnectionException("Could not find ServerMBean for server " + serverName + " to match PerfMBean on WebSphere Application Server.  ");
  }

  protected Object[] getStatConfigs(Object perfOName)
  {
    if (this.debug) System.err.println("Entering WebSphereMonitorJMX.getStatConfigs() with perfOName=" + perfOName + ".");
    if (this.statConfigs != null) {
      return this.statConfigs;
    }
    try
    {
      if (perfOName != null) {
        if (this.debug) System.err.println("WebSphereMonitorJMX.getStatConfigs() perfOName was null, invoking the AdminClient.getConfigs() method.");
        this.statConfigs = ((Object[])(Object[])this.management.invokeMethod.invoke(this.adminClient, new Object[] { perfOName, "getConfigs", null, null }));
      }
    } catch (Exception e) {
      error("WebSphereMonitorJMX.getStatConfigs(): " + e, e);
    }

    if (this.debug) System.err.println("Leaving WebSphereMonitorJMX.getStatConfigs() with statConfigs=" + this.statConfigs + ".");
    return this.statConfigs;
  }

  protected void bindConfigs(Object stats, Object[] configs) throws IllegalAccessException, InvocationTargetException {
    if (this.debug) System.err.println("Entering WebSphereMonitorJMX.bindConfigs() with stats=" + stats + " and configs=" + configs + ".");
    HashMap configMap = new HashMap();
    for (int i = 0; i < configs.length; ++i) {
      String shortName = (String)this.management.getShortNameMethod.invoke(configs[i], null);
      configMap.put(shortName, configs[i]);
    }
    Object[] subStats = (Object[])(Object[])this.management.getSubStatsMethod.invoke(stats, null);
    if (subStats != null) {
      for (int i = 0; i < subStats.length; ++i) {
        String statsName = (String)this.management.getStatsNameMethod.invoke(subStats[i], null);
        this.management.setConfigMethod.invoke(subStats[i], new Object[] { configMap.get(statsName) });
      }
    }
    if (!(this.debug)) return; System.err.println("Leaving WebSphereMonitorJMX.bindConfigs().");
  }

  protected Object collectStats() {
    return collectStatsWorker(true);
  }

  protected Object collectStatsWorker(boolean firstTry) {
    if (this.debug) System.err.println("Entering WebSphereMonitorJMX.collectStats().");
    try {
      Object[] statsObjectParams = { this.serverOName, Boolean.TRUE };
      String[] statsObjectSignature = { "javax.management.ObjectName", "java.lang.Boolean" };
      Object mbeanStats = this.management.invokeMethod.invoke(this.adminClient, new Object[] { this.perfOName, "getStatsObject", statsObjectParams, statsObjectSignature });

      Object[] configs = (Object[])(Object[])this.management.invokeMethod.invoke(this.adminClient, new Object[] { this.perfOName, "getConfigs", null, null });

      bindConfigs(mbeanStats, configs);

      if (this.debug) System.err.println("Leaving WebSphereMonitorJMX.collectStats() and returning mbeanStats=" + mbeanStats + ".");
      return mbeanStats;
    }
    catch (InvocationTargetException e)
    {
      if (firstTry) {
        return collectStatsWorker(false);
      }
      error("Exception in WebSphereMonitorJMX.collectStats(): " + e, e);
    }
    catch (Exception e) {
      error("Exception in WebSphereMonitorJMX.collectStats(): " + e, e);
    }
    if (this.debug) System.err.println("Leaving WebSphereMonitorJMX.collectStats() and returning null.");
    return null;
  }

  static
  {
    FileInputStream propFileInputStream = null;
    try {
      propFileInputStream = new FileInputStream(propsFilename);
      props.load(propFileInputStream);
    } catch (Exception e) {
      System.err.println("Cannot open Websphere properties file: " + propsFilename);
    } finally {
      try {
        if (propFileInputStream != null) propFileInputStream.close();
      } catch (Exception e) {
        System.err.println("Exception: " + e);
      }
    }
  }
}