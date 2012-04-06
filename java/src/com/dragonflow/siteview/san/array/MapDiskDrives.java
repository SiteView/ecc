package com.dragonflow.siteview.san.array;

import java.util.Calendar;
import java.util.Set;
import java.util.Vector;
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

public class MapDiskDrives
{
  //private Logger logger;
  private String CN = "MapDiskDrives";
  private CIM_DataTypes cim_DT = new CIM_DataTypes();
  private CIM_Qualifiers cim_Q = new CIM_Qualifiers();
  private PerformanceMetrics pm = new PerformanceMetrics();
  private WBEMClient cc;
  private CIMObjectPath instanceCOP;
  CIM_ComputerSystem ComputerSystem; 
 // private Integer computerSystemID;
  //private Session sessionMapDiskDrives;
  private CIM_StorageExtent cse;
  private CIM_PhysicalPackage cpp;
  private CIM_BlockStorageStatisticalData cbssd;
  
  CIM_DiskDrive tempDiskDrive;

  public MapDiskDrives( WBEMClient cc1, CIMObjectPath instanceCOP1, CIM_ComputerSystem ComputerSystem)
  {
    //this.logger = Logger.getLogger(this.CN);
   // this.sessionMapDiskDrives = session1;
    this.cc = cc1;
    this.instanceCOP = instanceCOP1;
    this.ComputerSystem = ComputerSystem;
    mapDiskDriveData();
  }

  public boolean mapDiskDriveData()
  {
    try
    {
      long instanceTimeMeanDisk = 0L;
      try {
        instanceTimeMeanDisk = this.pm.AssociatorsTime(this.cc, this.instanceCOP, 
          "CIM_SystemDevice", "CIM_DiskDrive", 
          "GroupComponent", "PartComponent");
      }
      catch (Exception localException1)
      {
      }

      CloseableIterator cim_DiskDriveEnum = this.cc.associators(this.instanceCOP, 
        "CIM_SystemDevice", "CIM_DiskDrive", 
        "GroupComponent", "PartComponent", false, false, 
        null);

      while (cim_DiskDriveEnum.hasNext())
      {
        CIMInstance assocCI = (CIMInstance)cim_DiskDriveEnum.next();
        CIMObjectPath diskDriveCOP = assocCI.getObjectPath();
        String enumerationDiskDriveSCCNKey;
        try
        {
          enumerationDiskDriveSCCNKey = this.cim_DT.getCIMInstancePropertyValueString(assocCI, "SystemCreationClassName");
        } catch (Exception e) {
          enumerationDiskDriveSCCNKey = null;
        }
        String enumerationDiskDriveSNKey;
        try {
          enumerationDiskDriveSNKey = this.cim_DT.getCIMInstancePropertyValueString(assocCI, "SystemName");
        } catch (Exception e1) {
          enumerationDiskDriveSNKey = null;
        }
        String enumerationDiskDriveCCNKey;
        try {
          enumerationDiskDriveCCNKey = this.cim_DT.getCIMInstancePropertyValueString(assocCI, "CreationClassName");
        } catch (Exception e1) {
          enumerationDiskDriveCCNKey = null;
        }
        String enumerationDiskDriveDIDKey;
        try {
          enumerationDiskDriveDIDKey = this.cim_DT.getCIMInstancePropertyValueString(assocCI, "DeviceID");
        } catch (Exception e1) {
          enumerationDiskDriveDIDKey = null;
        }

        tempDiskDrive = new CIM_DiskDrive(enumerationDiskDriveSCCNKey, enumerationDiskDriveSNKey, enumerationDiskDriveCCNKey, enumerationDiskDriveDIDKey);

        int instancePropertySize = assocCI.getPropertyCount();
        tempDiskDrive.setInstancePropertySize(instancePropertySize);

        tempDiskDrive.setInstanceTimeMean(Long.valueOf(instanceTimeMeanDisk));
        try
        {
          UnsignedInteger16[] operationalStatusArray = this.cim_DT.getUint16ArrayPropertyValue(assocCI, "OperationalStatus");
          int operationalStatusSize = 0;
          if (operationalStatusArray != null) {
            operationalStatusSize = operationalStatusArray.length;
          }
         // this.logger.debug("opearationalStatusSize = " + operationalStatusSize);
          Vector operationalStatusString = new Vector();
          for (int x = 0; x < operationalStatusSize; ++x)
          {
            UnsignedInteger16 opstsint = operationalStatusArray[x];

            int operationalStatusInt = Integer.parseInt(opstsint.toString());
            String operationalStatusValue = this.cim_Q.diskOperationalStatus(operationalStatusInt);

            operationalStatusString.add(operationalStatusValue);
          }

          String operationalStatusFinal = this.cim_Q.buildStringFromVector(operationalStatusString, ",");
          tempDiskDrive.setOperationalStatus(operationalStatusFinal);
        } catch (Exception e) {
        	tempDiskDrive.setOperationalStatus("Unknown");
          //this.logger.error("Operational Status", e);
        }
        try
        {
          Long cim_DiskDrivePropertyMaxMediaSize = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(assocCI, "MaxMediaSize").longValue());
          tempDiskDrive.setMaxMediaSize(cim_DiskDrivePropertyMaxMediaSize);
        }
        catch (Exception localException2) {
        }
        String elementName;
        try {
          elementName = this.cim_DT.getCIMInstancePropertyValueString(assocCI, "ElementName");
        } catch (Exception e) {
          elementName = null;
        }
        tempDiskDrive.setElementName(elementName);
        
        //-----------------------
        
        this.ComputerSystem.getCim_DiskDrive().add(tempDiskDrive);

        /*
        this.sessionMapDiskDrives.save(cdd);
        String diskDriveID = this.sessionMapDiskDrives.getIdentifier(cdd).toString();
        Integer diskDriveIDp = Integer.valueOf(diskDriveID);
        //this.logger.debug("diskDriveIDp = " + diskDriveIDp);
        //this.logger.debug("computerSystemID = " + this.computerSystemID);
        CIM_ComputerSystem aComputerSystem = (CIM_ComputerSystem)this.sessionMapDiskDrives.load(CIM_ComputerSystem.class, this.computerSystemID);
        CIM_DiskDrive aDiskDrive = (CIM_DiskDrive)this.sessionMapDiskDrives.load(CIM_DiskDrive.class, diskDriveIDp);
        aComputerSystem.getCim_DiskDrive().add(aDiskDrive);
        this.sessionMapDiskDrives.save(aComputerSystem);
        */

        if (cim_DiskDriveEnum != null) {
          CloseableIterator cim_PhysicalPackageEnum = this.cc.associators(diskDriveCOP, "CIM_Realizes", "CIM_PhysicalPackage", "Dependent", "Antecedent", false, false, null);
          while (cim_PhysicalPackageEnum.hasNext()) {
            CIMInstance cim_PhysicalPackageCI = (CIMInstance)cim_PhysicalPackageEnum.next();
            this.cpp = new CIM_PhysicalPackage();

            String manufacturer = "Not Available";
            try {
              manufacturer = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "Manufacturer");
            } catch (Exception e) {
              //this.logger.error(this.CN, e);
            }
            String model = "Not Available";
            try {
              model = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "Model");
            } catch (Exception e) {
              //this.logger.error(this.CN, e);
            }
            String version = "Not Available";
            try {
              version = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "Version");
            } catch (Exception e) {
              //this.logger.error(this.CN, e);
            }

            String serialNumber = "";
            try {
              serialNumber = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "SerialNumber");
            } catch (Exception e) {
              //this.logger.error(this.CN, e);
            }

            this.cpp.setManufacturer(manufacturer);
            this.cpp.setModel(model);
            this.cpp.setVersion(version);
            this.cpp.setSerialNumber(serialNumber);
            //--------------------
            this.tempDiskDrive.getCim_PhysicalPackage().add(this.cpp);
            
            
            /*
            this.sessionMapDiskDrives.save(this.cpp);
            String physicalPackageID = this.sessionMapDiskDrives.getIdentifier(this.cpp).toString();
            Integer physicalPackageIDp = Integer.valueOf(physicalPackageID);
           // this.logger.debug("physicalPackageIDp = " + physicalPackageID);
            CIM_DiskDrive aDiskDriveForPhysicalPackage = (CIM_DiskDrive)this.sessionMapDiskDrives.load(CIM_DiskDrive.class, diskDriveIDp);
            CIM_PhysicalPackage aPhysicalPackage = (CIM_PhysicalPackage)this.sessionMapDiskDrives.load(CIM_PhysicalPackage.class, physicalPackageIDp);
            aDiskDriveForPhysicalPackage.getCim_PhysicalPackage().add(aPhysicalPackage);
            this.sessionMapDiskDrives.save(aDiskDriveForPhysicalPackage);
            */
          }

          CloseableIterator cim_StorageExtentEnum = this.cc.associators(diskDriveCOP, "CIM_MediaPresent", "CIM_StorageExtent", "Antecedent", "Dependent", false, false, null);
          while (cim_StorageExtentEnum.hasNext()) {
            //this.logger.debug("StorageExtent");
            this.cse = new CIM_StorageExtent();
            CIMInstance cim_StorageExtentCI = (CIMInstance)cim_StorageExtentEnum.next();
            CIMObjectPath cim_StorageExtentInstanceCOP = cim_StorageExtentCI.getObjectPath();
            this.cse = new CIM_StorageExtent();
            Long blockSize = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StorageExtentCI, "BlockSize").longValue());
            Long numberOfBlocks = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StorageExtentCI, "NumberOfBlocks").longValue());

            this.cse.setBlockSize(blockSize);
            this.cse.setNumberOfBlocks(numberOfBlocks);
            
            //--------------
            this.tempDiskDrive.getCim_StorageExtent().add(this.cse);

            /*
            this.sessionMapDiskDrives.save(this.cse);
            String storageExtentID = this.sessionMapDiskDrives.getIdentifier(this.cse).toString();
            Integer storageExtentIDp = Integer.valueOf(storageExtentID);
            //this.logger.debug("storageExtentIDp = " + storageExtentID);
            CIM_DiskDrive aDiskDriveForStorageExtent = (CIM_DiskDrive)this.sessionMapDiskDrives.load(CIM_DiskDrive.class, diskDriveIDp);
            CIM_StorageExtent aStorageExtent = (CIM_StorageExtent)this.sessionMapDiskDrives.load(CIM_StorageExtent.class, storageExtentIDp);
            aDiskDriveForStorageExtent.getCim_StorageExtent().add(aStorageExtent);
            this.sessionMapDiskDrives.save(aDiskDriveForStorageExtent);
            */

            if (cim_StorageExtentEnum != null) {
              CloseableIterator cim_BlockStorageStatisticalDataEnum = null;
              try {
                cim_BlockStorageStatisticalDataEnum = this.cc.associators(cim_StorageExtentInstanceCOP, "CIM_ElementStatisticalData", "CIM_BlockStorageStatisticalData", "ManagedElement", "Stats", false, false, null);

                while (cim_BlockStorageStatisticalDataEnum.hasNext()) {
                  //this.logger.debug("BlockStorageStatisticalData");
                  CIMInstance cim_BlockStorageStatisticalDataCI = (CIMInstance)cim_BlockStorageStatisticalDataEnum.next();

                  this.cbssd = new CIM_BlockStorageStatisticalData();
                  int elementTypeBSD = 0;
                  try {
                    elementTypeBSD = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_BlockStorageStatisticalDataCI, "ElementType").intValue();
                    this.cbssd.setElementType(elementTypeBSD);
                  }
                  catch (NullPointerException localNullPointerException1)
                  {
                  }
                  Long totalIOs;
                  try {
                    totalIOs = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "TotalIOs").longValue());
                  } catch (NullPointerException npe) {
                    totalIOs = null;
                  }
                  this.cbssd.setTotalIos(totalIOs);
                  Long kBytesTransferred;
                  try
                  {
                    kBytesTransferred = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "KBytesTransferred").longValue());
                  } catch (NullPointerException npe) {
                    kBytesTransferred = null;
                  }
                  this.cbssd.setKBytesTransferred(kBytesTransferred);
                  Long readIOs;
                  try
                  {
                    readIOs = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "ReadIOs").longValue());
                  } catch (NullPointerException npe) {
                    readIOs = null;
                  }
                  this.cbssd.setReadHitIos(readIOs);
                  Long readHitIOs;
                  try
                  {
                    readHitIOs = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "ReadHitIOs").longValue());
                  } catch (NullPointerException npe) {
                    readHitIOs = null;
                  }
                  this.cbssd.setReadHitIos(readHitIOs);
                  try
                  {
                    String instanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_BlockStorageStatisticalDataCI, "InstanceID");
                    this.cbssd.setInstanceId(instanceId);
                  }
                  catch (NullPointerException localNullPointerException2) {
                  }
                  try {
                    CIMDateTime statisticTime = this.cim_DT.getCIMInstancePropertyDateTime(cim_BlockStorageStatisticalDataCI, "StatisticTime");
                    Calendar cal = this.cim_DT.getCalendarFromCIMDateTime(statisticTime);
                    this.cbssd.setStatisticTime(cal);
                  } catch (Exception e) {
                    //this.logger.error("CS BSP Error", e);
                  }
                  //---------------------
                  
                  this.cse.getCim_BlockStorageStatisticalData().add(this.cbssd);

                  /*
                  this.sessionMapDiskDrives.save(this.cbssd);
                  String blockStorageStatisticalDataID = this.sessionMapDiskDrives.getIdentifier(this.cbssd).toString();
                  Integer blockStorageStatisticalDataIDp = Integer.valueOf(blockStorageStatisticalDataID);
                  //this.logger.debug("blockStorageStatisticalDataIDIDp = " + blockStorageStatisticalDataID);
                  CIM_StorageExtent aStorageExtentForBlockStorageStatisticalData = (CIM_StorageExtent)this.sessionMapDiskDrives.load(CIM_StorageExtent.class, storageExtentIDp);
                  CIM_BlockStorageStatisticalData aBlockStorageStatisticalData = (CIM_BlockStorageStatisticalData)this.sessionMapDiskDrives.load(CIM_BlockStorageStatisticalData.class, blockStorageStatisticalDataIDp);
                  aStorageExtentForBlockStorageStatisticalData.getCim_BlockStorageStatisticalData().add(aBlockStorageStatisticalData);
                  this.sessionMapDiskDrives.save(aStorageExtentForBlockStorageStatisticalData);
                  */

                 // CCalculationsBSPDisk calculateDiskDelta = new CCalculationsBSPDisk();
                 // calculateDiskDelta.calculateDiskMetric(this.computerSystemID.intValue(), diskDriveIDp.intValue(), aDiskDrive, this.cbssd);
                }
              }
              catch (WBEMException localWBEMException1)
              {
              }
            }
          }
        }
      }
      cim_DiskDriveEnum.close();
    } catch (WBEMException ce) {
      //this.logger.error("MapDiskDrives", ce);
      return false;
    }
    return true;
  }
}