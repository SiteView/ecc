package com.dragonflow.siteview.san.util;

import java.util.ArrayList;
import java.util.Calendar;
import javax.cim.CIMObjectPath;
import javax.wbem.CloseableIterator;
import javax.wbem.WBEMException;
import javax.wbem.client.WBEMClient;
//import org.apache.log4j.Logger;

public class PerformanceMetrics
{
//  private Logger logger;
  private String CN = "PerformanceMetrics";
  private CIM_DataTypes cim_DT;

  public PerformanceMetrics()
  {
//    this.logger = Logger.getLogger(this.CN);
    this.cim_DT = new CIM_DataTypes();
  }

  public long enumerationTime(WBEMClient cc, CIMObjectPath cop)
  {
    long statInstanceMean = 0L;
    try {
      Calendar statCalBefore = Calendar.getInstance();
      long msBefore = statCalBefore.getTimeInMillis();
//      this.logger.debug("msBefore = " + msBefore);
      CloseableIterator enumeration = cc.enumerateInstances(cop, false, false, false, null);
      Calendar statCalAfter = Calendar.getInstance();
      long msAfter = statCalAfter.getTimeInMillis();
//      this.logger.debug("msAfter = " + msAfter);
      ArrayList al = this.cim_DT.iteratorToInstanceArrayList(enumeration);
      int alSize = al.size();
//      this.logger.debug("Enumeration Size = " + alSize);
      statInstanceMean = (msAfter - msBefore) / alSize;
//      this.logger.debug("statInstanceMean = " + statInstanceMean);
      return statInstanceMean;
    }
    catch (WBEMException localWBEMException) {
    }
    return statInstanceMean;
  }

  public long AssociatorsTime(WBEMClient cc, CIMObjectPath cop, String associator, String destination, String keyFrom, String keyTo)
  {
    long statInstanceMean = 0L;
    try {
      Calendar statCalBefore = Calendar.getInstance();
      long msBefore = statCalBefore.getTimeInMillis();
//      this.logger.debug("msBefore = " + msBefore);
      CloseableIterator enumeration = cc.associators(cop, associator, destination, keyFrom, keyTo, false, false, null);
      Calendar statCalAfter = Calendar.getInstance();
      long msAfter = statCalAfter.getTimeInMillis();
//      this.logger.debug("msAfter = " + msAfter);
      ArrayList al = this.cim_DT.iteratorToInstanceArrayList(enumeration);
      int alSize = al.size();
//      this.logger.debug("Enumeration Size = " + alSize);
      statInstanceMean = (msAfter - msBefore) / alSize;
//      this.logger.debug("statInstanceMean = " + statInstanceMean);
      return statInstanceMean;
    }
    catch (Exception localException) {
    }
    return statInstanceMean;
  }
}