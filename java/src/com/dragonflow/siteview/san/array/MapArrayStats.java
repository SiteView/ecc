package com.dragonflow.siteview.san.array;

import java.util.Calendar;
import java.util.Set;
import javax.cim.CIMDateTime;
import javax.cim.CIMInstance;
import javax.cim.CIMObjectPath;
import javax.cim.UnsignedInteger16;
import javax.cim.UnsignedInteger64;
import javax.wbem.CloseableIterator;
import javax.wbem.WBEMException;
import javax.wbem.client.WBEMClient;
//import org.hibernate.Session;

import com.dragonflow.siteview.san.beans.*;
import com.dragonflow.siteview.san.util.*;


public class MapArrayStats
{
 // private Logger logger;
  private String CN = "MapArrayStats";
  private CIM_DataTypes cim_DT = new CIM_DataTypes();
  private WBEMClient cc;
  private CIMObjectPath instanceCOP;
 // private Integer computerSystemID;
//  private Session sessionMapArrayStats;
  CIM_ComputerSystem ComputerSystem;

  public MapArrayStats( WBEMClient cc1, CIMObjectPath instanceCOP1, CIM_ComputerSystem ComputerSystem)
  {
    //this.logger = Logger.getLogger(this.CN);
    //this.sessionMapArrayStats = session1;
    this.cc = cc1;
    this.instanceCOP = instanceCOP1;
    this.ComputerSystem=ComputerSystem;
    //this.computerSystemID = computerSystemID1;
    //this.logger.debug("Session1 = " + session1);
    //this.logger.debug("cc1 = " + cc1);
    //this.logger.debug("instaceCOP1 = " + instanceCOP1);
    //this.logger.debug("ComputerSystemID1 = " + computerSystemID1);
    mapSoftwareIdentity();
  }

  public void mapSoftwareIdentity()
  {
    try {
      CloseableIterator cim_BlockStorageStatisticalData = this.cc
        .associators(this.instanceCOP, "CIM_ElementStatisticalData", "CIM_BlockStorageStatisticalData", 
        "ManagedElement", "Stats", false, false, null);

      while (cim_BlockStorageStatisticalData.hasNext()) {
        //this.logger.debug("enumerated BlockStorageStatisticalData and has more elements");
        CIMInstance cim_BlockStorageStatisticalDataCI = (CIMInstance)cim_BlockStorageStatisticalData.next();

        CIM_BlockStorageStatisticalData cbssd = new CIM_BlockStorageStatisticalData();
        if (cim_BlockStorageStatisticalData == null)
          continue;
        try
        {
          String instanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_BlockStorageStatisticalDataCI, "InstanceID");
          cbssd.setInstanceId(instanceId);
        } catch (Exception e) {
          cbssd.setInstanceId(null);
        }
        try {
          int elementType = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_BlockStorageStatisticalDataCI, "ElementType").intValue();
          cbssd.setElementType(elementType);
        }
        catch (Exception localException1) {
        }
        Long writeIOs;
        try {
          writeIOs = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "WriteIOs").longValue());
        } catch (NullPointerException npe) {
          writeIOs = null;
        }
        cbssd.setWriteIos(writeIOs);
        Long readIOs;
        try {
          readIOs = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "ReadIOs").longValue());
        } catch (NullPointerException e) {
          readIOs = null;
        }
        cbssd.setReadIos(readIOs);
        Long kBytesTransferred;
        try {
          kBytesTransferred = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "KBytesTransferred").longValue());
        } catch (NullPointerException e) {
          kBytesTransferred = null;
        }
        cbssd.setKBytesTransferred(kBytesTransferred);
        Long totalIOs;
        try {
          totalIOs = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "TotalIOs").longValue());
        } catch (RuntimeException e) {
          totalIOs = null;
        }
        cbssd.setTotalIos(totalIOs);
        try {
          CIMDateTime statisticTime = this.cim_DT.getCIMInstancePropertyDateTime(cim_BlockStorageStatisticalDataCI, "StatisticTime");
          Calendar cal = this.cim_DT.getCalendarFromCIMDateTime(statisticTime);
          cbssd.setStatisticTime(cal);
        } catch (Exception e) {
          //this.logger.error("CS BSP Error", e);
        }
        //---------
        
        this.ComputerSystem.getCim_BlockStorageStatisticalData().add(cbssd);

        /*
        this.sessionMapArrayStats.save(cbssd);

        String blockStorageStatisticalDataID = this.sessionMapArrayStats.getIdentifier(cbssd).toString();
        Integer blockStorageStatisticalDataIDp = Integer.valueOf(blockStorageStatisticalDataID);
       // this.logger.debug("blockStorageStatisticalDataIDp = " + blockStorageStatisticalDataIDp);
        //this.logger.debug("blockStorageStatisticalDataID = " + blockStorageStatisticalDataID);
        CIM_ComputerSystem aComputerSystemForStats = (CIM_ComputerSystem)this.sessionMapArrayStats.load(CIM_ComputerSystem.class, this.computerSystemID);
        CIM_BlockStorageStatisticalData aBlockStorageStatisticalData = (CIM_BlockStorageStatisticalData)this.sessionMapArrayStats.load(CIM_BlockStorageStatisticalData.class, blockStorageStatisticalDataIDp);
        aComputerSystemForStats.getCim_BlockStorageStatisticalData().add(aBlockStorageStatisticalData);
        this.sessionMapArrayStats.save(aComputerSystemForStats);
        */
      }

    }
    catch (WBEMException ce)
    {
      //this.logger.error("MapArrayStats", ce);
    }
  }
}