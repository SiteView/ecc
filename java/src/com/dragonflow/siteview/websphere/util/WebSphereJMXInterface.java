package com.dragonflow.siteview.websphere.util;
import java.io.PrintStream;
import java.lang.reflect.Method;

import com.dragonflow.erlangecc.monitor.JMXInterface;
public class WebSphereJMXInterface extends JMXInterface
{
  private WebSphereConnectionProperties connProps;
  public Class AdminClientClass;
  public Class StatsClass;
  public Class PMIModuleConfigClass;
  public Class PMIDataInfoClass;
  public Class StatsImplClass;
  public Class PMIClientClass;
  public Class StatisticClass;
  public Class BoundaryStatisticImplClass;
  public Class AverageStatisticImplClass;
  public Class BoundedRangeStatisticImplClass;
  public Class CountStatisticImplClass;
  public Class DoubleStatisticImplClass;
  public Class RangeStatisticImplClass;
  public Class TimeStatisticImplClass;
  public Method getStatisticNamesMethod;
  public Method getShortNameMethod;
  public Method getMBeanTypeMethod;
  public Method listAllDataMethod;
  public Method getDICategoryMethod;
  public Method getDICommentMethod;
  public Method getDIDescriptionMethod;
  public Method getDINameMethod;
  public Method setConfigMethod;
  public Method getStatisticMethod;
  public Method dataMembersMethod;
  public Method getStatisticsMethod;
  public Method getSubStatsMethod;
  public Method getStatsNameMethod;
  public Method getStatsMethod;
  public Method getStatisticNameMethod;
  public Method getLowerBoundMethod;
  public Method getUpperBoundMethod;
  public Method getCurrentMethod;
  public Method getCountMethod;
  public Method getDoubleMethod;
  public Method getRSCurrentMethod;
  public Method getTotalMethod;
  public Method getTimeCountMethod;
  public Method getAverageMethod;
  public Method getNLSValueMethod;

  public WebSphereJMXInterface(Class adminClientClass, WebSphereConnectionProperties connectionProps)
  {
    this.connProps = connectionProps;

    String webspherePackageName = "ws";
    if (this.connProps.getVersion() == 2) {
      webspherePackageName = "websphere";
    }

    ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
    try {
      this.AdminClientClass = adminClientClass;
      this.queryNamesMethod = adminClientClass.getMethod("queryNames", new Class[] { this.ObjectNameClass, this.QueryExpClass });
      this.getAttributeMethod = adminClientClass.getMethod("getAttribute", new Class[] { this.ObjectNameClass, String.class });
      this.getMBeanInfoMethod = adminClientClass.getMethod("getMBeanInfo", new Class[] { this.ObjectNameClass });
      Class stringClass = String.class;
      Class objArrClass = java.lang.Object[].class;
      Class strArrClass =java.lang.String[].class;
      this.invokeMethod = adminClientClass.getMethod("invoke", new Class[] { this.ObjectNameClass, stringClass, objArrClass, strArrClass });

      this.StatsClass = classLoader.loadClass("com.ibm.websphere.management.statistics.Stats");
      this.PMIModuleConfigClass = classLoader.loadClass("com.ibm.websphere.pmi.PmiModuleConfig");
      this.PMIDataInfoClass = classLoader.loadClass("com.ibm.websphere.pmi.PmiDataInfo");
      this.StatsImplClass = classLoader.loadClass("com.ibm." + webspherePackageName + ".pmi.stat.StatsImpl");

      this.getStatisticNamesMethod = this.StatsClass.getMethod("getStatisticNames", null);

      this.getShortNameMethod = this.PMIModuleConfigClass.getMethod("getShortName", null);
      this.getMBeanTypeMethod = this.PMIModuleConfigClass.getMethod("getMbeanType", null);
      this.listAllDataMethod = this.PMIModuleConfigClass.getMethod("listAllData", null);

      this.getDICategoryMethod = this.PMIDataInfoClass.getMethod("getCategory", null);
      this.getDICommentMethod = this.PMIDataInfoClass.getMethod("getComment", null);
      this.getDIDescriptionMethod = this.PMIDataInfoClass.getMethod("getDescription", null);
      this.getDINameMethod = this.PMIDataInfoClass.getMethod("getName", null);

      this.setConfigMethod = this.StatsImplClass.getMethod("setConfig", new Class[] { this.PMIModuleConfigClass });
      this.getStatisticMethod = this.StatsImplClass.getMethod("getStatistic", new Class[] { String.class });
      this.dataMembersMethod = this.StatsImplClass.getMethod("dataMembers", null);
      this.getStatisticsMethod = this.StatsImplClass.getMethod("getStatistics", null);
      this.getSubStatsMethod = this.StatsImplClass.getMethod("getSubStats", null);
      this.getStatsNameMethod = this.StatsImplClass.getMethod("getName", null);
      this.getStatsMethod = this.StatsImplClass.getMethod("getStats", new Class[] { String.class });

      if (this.connProps.getVersion() == 2)
        this.StatisticClass = classLoader.loadClass("com.ibm.websphere.management.statistics.Statistic");
      else {
        this.StatisticClass = classLoader.loadClass("com.ibm.ws.pmi.stat.StatisticImpl");
      }

      this.BoundedRangeStatisticImplClass = classLoader.loadClass("com.ibm." + webspherePackageName + ".pmi.stat.BoundedRangeStatisticImpl");
      this.BoundaryStatisticImplClass = classLoader.loadClass("com.ibm." + webspherePackageName + ".pmi.stat.BoundaryStatisticImpl");
      this.CountStatisticImplClass = classLoader.loadClass("com.ibm." + webspherePackageName + ".pmi.stat.CountStatisticImpl");
      this.DoubleStatisticImplClass = classLoader.loadClass("com.ibm." + webspherePackageName + ".pmi.stat.DoubleStatisticImpl");
      this.RangeStatisticImplClass = classLoader.loadClass("com.ibm." + webspherePackageName + ".pmi.stat.RangeStatisticImpl");
      this.TimeStatisticImplClass = classLoader.loadClass("com.ibm." + webspherePackageName + ".pmi.stat.TimeStatisticImpl");
      try
      {
        this.AverageStatisticImplClass = classLoader.loadClass("com.ibm." + webspherePackageName + ".pmi.stat.AverageStatisticImpl");
      }
      catch (Exception e) {
        System.out.println("Exception: " + e);
      }

      this.getStatisticNameMethod = this.StatisticClass.getMethod("getName", null);

      this.getLowerBoundMethod = this.BoundaryStatisticImplClass.getMethod("getLowerBound", null);
      this.getUpperBoundMethod = this.BoundaryStatisticImplClass.getMethod("getUpperBound", null);

      this.getCurrentMethod = this.BoundedRangeStatisticImplClass.getMethod("getCurrent", null);

      this.getCountMethod = this.CountStatisticImplClass.getMethod("getCount", null);

      this.getDoubleMethod = this.DoubleStatisticImplClass.getMethod("getDouble", null);

      this.getRSCurrentMethod = this.RangeStatisticImplClass.getMethod("getCurrent", null);

      this.getTotalMethod = this.TimeStatisticImplClass.getMethod("getTotal", null);

      this.getTimeCountMethod = this.TimeStatisticImplClass.getMethod("getCount", null);

      if (this.AverageStatisticImplClass != null) {
        this.getAverageMethod = this.AverageStatisticImplClass.getMethod("getMean", null);
      }

      this.PMIClientClass = classLoader.loadClass("com.ibm.websphere.pmi.client.PmiClient");
      this.getNLSValueMethod = this.PMIClientClass.getMethod("getNLSValue", new Class[] { String.class });
    }
    catch (Exception e) {
      System.err.println("Failed to load javax.management class or method: " + e.toString());
    }
  }

  public int getVersion() {
    return this.connProps.getVersion();
  }
}
