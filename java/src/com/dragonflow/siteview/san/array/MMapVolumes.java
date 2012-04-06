package com.dragonflow.siteview.san.array;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;
import javax.cim.CIMDateTime;
import javax.cim.CIMInstance;
import javax.cim.CIMObjectPath;
import javax.cim.UnsignedInteger16;
import javax.cim.UnsignedInteger64;
import javax.cim.UnsignedInteger8;
import javax.wbem.CloseableIterator;
import javax.wbem.WBEMException;
import javax.wbem.client.WBEMClient;
//import org.hibernate.Session;

import com.dragonflow.siteview.san.beans.*;
import com.dragonflow.siteview.san.util.*;

public class MMapVolumes
{
  //private Logger logger;
  private String CN = "MapVolumes";

  private CIM_DataTypes cim_DT = new CIM_DataTypes();
  private CIM_Qualifiers cim_Q = new CIM_Qualifiers();
  PerformanceMetrics pm = new PerformanceMetrics();
  private WBEMClient cc;
  private CIMObjectPath instanceCOP;
  private String flag="";
 // private Integer computerSystemID;
 // private Session sessionMapVolumes;
  public CIM_ComputerSystem ComputerSystem;


  public Set<CIM_StoragePool> v7000StoragePools = new HashSet();
  public List<CIM_StorageVolume> v7000StorageVolumes=new ArrayList<CIM_StorageVolume>();
  
  public MMapVolumes(WBEMClient cc1, CIMObjectPath instanceCOP1, CIM_ComputerSystem ComputerSystem,String flag)
  {
  //  this.logger = Logger.getLogger(this.CN);
   // this.sessionMapVolumes = session1;
    this.cc = cc1;
    this.instanceCOP = instanceCOP1;
    this.ComputerSystem=ComputerSystem;
    this.flag=flag;
   // this.computerSystemID = computerSystemID1;
   // this.logger.debug("Session1 = " + session1);
   // this.logger.debug("cc1 = " + cc1);
   // this.logger.debug("instaceCOP1 = " + instanceCOP1);
   // this.logger.debug("ComputerSystemID1 = " + computerSystemID1);
   // this.logger.debug("");
    if (flag.equals("v7000"))
    {
    	v7000mapVolumeData();
    }else
    {	
        mapVolumeData();
    }

  }
  public boolean v7000mapVolumeData()
  {

	    try {
	    

	      CloseableIterator cim_StoragePoolEnum = this.cc.associators(this.instanceCOP, 
	        "CIM_HostedStoragePool", "CIM_StoragePool", 
	        "GroupComponent", "PartComponent", false, false, 
	        null);

	      while (cim_StoragePoolEnum.hasNext())
	      {
	     //   this.logger.debug(this.CN + " Enumerated StoragePool and has more elements");
	        CIMInstance cim_StoragePoolCI = (CIMInstance)cim_StoragePoolEnum.next();
	        CIMObjectPath cim_StoragePoolInstanceCOP = cim_StoragePoolCI.getObjectPath();
	        CIM_StoragePool csp = new CIM_StoragePool();

	        int instancePropertySize = cim_StoragePoolCI.getPropertyCount();
	        csp.setInstancePropertySize(instancePropertySize);
	     //   this.logger.info("InstancePropertySize = " + instancePropertySize);
	      //  csp.setInstanceTimeMean(Long.valueOf(instanceTimeMeanPool));

	        if (cim_StoragePoolEnum != null) {
	          String instanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_StoragePoolCI, "InstanceID");
	          String elementName;
	          try {
	            elementName = this.cim_DT.getCIMInstancePropertyValueString(cim_StoragePoolCI, "ElementName");
	          } catch (Exception e4) {
	            elementName = "Unknown";
	          }
	          boolean primordial;
	          try {
	            primordial = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_StoragePoolCI, "Primordial").booleanValue();
	          } catch (Exception e) {
	            primordial = false;
	          }

	          try
	          {
	            UnsignedInteger16[] operationalStatusArray = this.cim_DT.getUint16ArrayPropertyValue(cim_StoragePoolCI, "OperationalStatus");
	            int operationalStatusSize = 0;
	            if (operationalStatusArray != null) {
	              operationalStatusSize = operationalStatusArray.length;
	            }
	         //   this.logger.debug("opearationalStatusSize = " + operationalStatusSize);
	            Vector operationalStatusString = new Vector();
	            for (int x = 0; x < operationalStatusSize; ++x)
	            {
	              UnsignedInteger16 opstsint = operationalStatusArray[x];

	              int operationalStatusInt = Integer.parseInt(opstsint.toString());
	              String operationalStatusValue = this.cim_Q.diskOperationalStatus(operationalStatusInt);

	              operationalStatusString.add(operationalStatusValue);
	            }

	            String operationalStatusFinal = this.cim_Q.buildStringFromVector(operationalStatusString, ",");
	            csp.setOperationalStatus(operationalStatusFinal);
	          } catch (Exception operationalStatusArray) {
	            csp.setOperationalStatus("Unknown");
	          //  this.logger.error("Operational Status", e);
	          }

	          String poolId = "Not Available";
	          try {
	            poolId = this.cim_DT.getCIMInstancePropertyValueString(cim_StoragePoolCI, "PoolID");
	          } catch (Exception e) {
	          //  this.logger.error(this.CN, e);
	          }
	          Long totalManagedSpace = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StoragePoolCI, "TotalManagedSpace").longValue());
	          Long remainingManagedSpace = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StoragePoolCI, "RemainingManagedSpace").longValue());

	          csp.setInstanceId(instanceId);
	          csp.setElementName(elementName);
	          csp.setPrimordial(primordial);
	          csp.setPoolId(poolId);
	          csp.setTotalManagedSpace(totalManagedSpace);
	          csp.setRemainingManagedSpace(remainingManagedSpace);

	          CloseableIterator cim_AllocatedFromStoragePoolEnum = this.cc.associators(cim_StoragePoolInstanceCOP, 
	            "CIM_AllocatedFromStoragePool", "CIM_StoragePool", 
	            "Dependent", "Antecedent", false, false, 
	            null);

	          while (cim_AllocatedFromStoragePoolEnum.hasNext()) {
	           // this.logger.debug(this.CN + " Enumerated AllocatedFromStoragePool and has more elements");
	            CIMInstance cim_AllocatedFromStoragePoolCI = (CIMInstance)cim_AllocatedFromStoragePoolEnum.next();

	            if (cim_AllocatedFromStoragePoolEnum != null) {
	              String allocatedFromStoragePoolInstanceId = null;
	              try {
	                allocatedFromStoragePoolInstanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_AllocatedFromStoragePoolCI, "InstanceID");
	                csp.setAllocatedFromStoragePool(allocatedFromStoragePoolInstanceId);
	              }
	              catch (Exception localException2)
	              {
	              }
	            }

	          }

	          //this.sessionMapVolumes.save(csp);
	         // this.csps.add(csp);
	          /*
	          String storagePoolID = this.sessionMapVolumes.getIdentifier(csp).toString();
	          Integer storagePoolIDp = Integer.valueOf(storagePoolID);
	         // this.logger.info("storagePoolIDp = " + storagePoolID);
	          CIM_ComputerSystem aComputerSystem = (CIM_ComputerSystem)this.sessionMapVolumes.load(CIM_ComputerSystem.class, this.computerSystemID);
	          CIM_StoragePool aStoragePool = (CIM_StoragePool)this.sessionMapVolumes.load(CIM_StoragePool.class, storagePoolIDp);
	          aComputerSystem.getCim_StoragePool().add(aStoragePool);
	          this.sessionMapVolumes.save(aComputerSystem);
	          */
	          //--------------------fill StoragePool------------
	          this.ComputerSystem.getCim_StoragePool().add(csp);
	          if((!primordial)&&(elementName.startsWith("V7000_pool")))
	          {
	        	  this.v7000StoragePools.add(csp);
	        	  
	          }

	          CloseableIterator cim_StorageVolumeEnum = this.cc
	            .associators(cim_StoragePoolInstanceCOP, 
	            "CIM_AllocatedFromStoragePool", 
	            "CIM_StorageVolume", "Antecedent", 
	            "Dependent", false, false, null);
	          while (cim_StorageVolumeEnum.hasNext())
	          {
	          //  this.logger.info(this.CN + " Enumerated storagevolume and has more elements");
	            CIMInstance cim_StorageVolumeCI = (CIMInstance)cim_StorageVolumeEnum.next();
	            CIMObjectPath cim_StorageVolumeCOP = cim_StorageVolumeCI.getObjectPath();
	            CIM_StorageVolume csv = new CIM_StorageVolume();
	            if (cim_StorageVolumeEnum == null) {
	              continue;
	            }
	          //  csv.setInstanceTimeMean(Long.valueOf(statInstanceMeanVolume));
	            int instancePropertySizeVolume = cim_StorageVolumeCI.getPropertyCount();
	            csv.setInstancePropertySize(instancePropertySizeVolume);
	          //  this.logger.debug("InstancePropertySizeVolume = " + instancePropertySizeVolume);
	            String deviceId;
	            try
	            {
	              deviceId = this.cim_DT.getCIMInstancePropertyValueString(cim_StorageVolumeCI, "DeviceID");
	            } catch (RuntimeException e3) {
	              deviceId = null;
	            }

	            int enabledDefault = 0;
	            try {
	              enabledDefault = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "EnabledDefault").intValue();
	            } catch (RuntimeException e) {
	          //    this.logger.debug("No EnabledDefault for volume");
	            }

	            boolean seqentialAccess = false;

	            boolean seqentialAccessDiscovery = true;
	            try {
	              seqentialAccess = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_StorageVolumeCI, "SeqentialAccess").booleanValue();
	            } catch (Exception npe) {
	              seqentialAccessDiscovery = false;
	            }

	            int nameNameSpace = 0;
	            boolean nameNameSpaceDiscovery = true;
	            try {
	              nameNameSpace = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "NameNamespace").intValue();
	            } catch (Exception npe) {
	              nameNameSpaceDiscovery = false;
	            }

	            int nameFormat = 0;
	            try {
	              nameFormat = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "NameFormat").intValue();
	            } catch (Exception e) {
	           //   this.logger.debug("No NameFormat for Volume");
	            }
	            String name;
	            try
	            {
	              name = this.cim_DT.getCIMInstancePropertyValueString(cim_StorageVolumeCI, "Name");
	            } catch (Exception e2) {
	              name = null;
	            }
	            boolean primordialVolume;
	            try {
	              primordialVolume = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_StorageVolumeCI, "Primordial").booleanValue();
	            } catch (Exception e1) {
	              primordialVolume = false;
	            }

	            int deltaReservation = 0;
	            try {
	              deltaReservation = this.cim_DT.getCIMInstancePropertyUnsignedInt8Value(cim_StorageVolumeCI, "DeltaReservation").intValue();
	            } catch (Exception e) {
	             // this.logger.debug("No DeltaReservation for Volume");
	            }

	            int packageRedundancy = 0;
	            try {
	              packageRedundancy = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "PackageRedundancy").intValue();
	            } catch (Exception e) {
	            //  this.logger.debug("No Package Redundancy for Volume");
	            }

	            int dataRedundancy = 0;
	            try {
	              dataRedundancy = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "DataRedundancy").intValue();
	            } catch (Exception e) {
	            //  this.logger.debug("No DataRedundancy for Volume");
	            }
	            boolean noSinglePointOfFailure;
	            try {
	              noSinglePointOfFailure = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_StorageVolumeCI, "NoSinglePointOfFailure").booleanValue();
	            } catch (Exception e) {
	              noSinglePointOfFailure = false;
	            }

	            try
	            {
	              UnsignedInteger16[] extentStatus = this.cim_DT.getUint16ArrayPropertyValue(cim_StoragePoolCI, "ExtentStatus");
	              int extentStatusSize = 0;
	              if (extentStatus != null) {
	                extentStatusSize = extentStatus.length;
	              }
	             // this.logger.debug("ExtentStatusDiskSize = " + extentStatusSize);
	              Vector extentStatusString = new Vector();
	              for (int x = 0; x < extentStatusSize; ++x)
	              {
	                int extentStatusInt = Integer.parseInt(extentStatus[x].toString());

	                String extentStatusValue = this.cim_Q.extentStatus(extentStatusInt);

	                extentStatusString.add(extentStatusValue);
	              }

	              String extentStatusFinal = this.cim_Q.buildStringFromVector(extentStatusString, ",");
	              csv.setExtentStatus(extentStatusFinal);
	            } catch (Exception extentStatus) {
	             // this.logger.error("ExtentStatus", e);
	              csv.setExtentStatus("Unknown");
	            }
	            boolean isBasedOnUnderlyingRed;
	            try
	            {
	              isBasedOnUnderlyingRed = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_StorageVolumeCI, "IsBasedOnUnderlyingRedundancy").booleanValue();
	            } catch (Exception e) {
	              isBasedOnUnderlyingRed = false;
	            }
	            Long consumableBlocks;
	            try {
	              consumableBlocks = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StorageVolumeCI, "ConsumableBlocks").longValue());
	            } catch (NullPointerException npe) {
	              consumableBlocks = null;
	            }
	            Long numberOfBlocks = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StorageVolumeCI, "NumberOfBlocks").longValue());
	            Long blockSize = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StorageVolumeCI, "BlockSize").longValue());

	            int access = 555;
	            boolean accessDiscovery = true;
	            try {
	              access = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "Access").intValue();
	            } catch (NullPointerException npe) {
	              accessDiscovery = false;
	            }
	            String purpose = "Not Available";
	            try {
	              purpose = this.cim_DT.getCIMInstancePropertyValueString(cim_StorageVolumeCI, "Purpose");
	            } catch (Exception e) {
	            //  this.logger.error(this.CN, e);
	            }
	            String poolName = "Not Available";
	            try {
	            	poolName = this.cim_DT.getCIMInstancePropertyValueString(cim_StorageVolumeCI, "PoolName");
	            } catch (Exception e) {
	            //  this.logger.error(this.CN, e);
	            }

	            String identifyingDescriptions = null;
	            try
	            {
	              String[] identifyingDescriptionsArray = this.cim_DT.getStringArrayPropertyValue(cim_StorageVolumeCI, "IdentifyingDescriptions");
	              int identifyingDescriptionsSize = 0;
	              if (identifyingDescriptionsArray != null) {
	                identifyingDescriptionsSize = identifyingDescriptionsArray.length;
	              }

	             // this.logger.debug("identifyingDescriptionsSize = " + identifyingDescriptionsSize);
	              String identifyingDescriptions1=null;
	              if (identifyingDescriptionsSize == 1) {
	                identifyingDescriptions1 = identifyingDescriptionsArray[0].toString();
	                identifyingDescriptions=identifyingDescriptions1;
	              }
	              if (identifyingDescriptionsSize == 2) {
	                identifyingDescriptions1 = identifyingDescriptionsArray[0].toString();

	                String identifyingDescriptions2 = identifyingDescriptionsArray[1].toString();

	                identifyingDescriptions = identifyingDescriptions1 + "," + identifyingDescriptions2;
	                //this.logger.debug("identifyingDescriptions1 = " + identifyingDescriptions1 + " identifyingDescriptions2 = " + identifyingDescriptions2);
	              }
	            } catch (Exception e) {
	            //  this.logger.error("ERROR", e);
	            }

	            String otherIdentifyingInfo = null;
	            try
	            {
	              String[] otherIdentifyingInfoArray = this.cim_DT.getStringArrayPropertyValue(cim_StorageVolumeCI, "OtherIdentifyingInfo");
	              int otherIdentifyingInfoSize = 0;
	              if (otherIdentifyingInfoArray != null) {
	                otherIdentifyingInfoSize = otherIdentifyingInfoArray.length;
	              }

	             // this.logger.debug("otherIdentifyingInfoSize = " + otherIdentifyingInfoSize);
	              String otherIdentifyingInfo1;
	              if (otherIdentifyingInfoSize == 1) {
	                otherIdentifyingInfo1 = otherIdentifyingInfoArray[0].toString();

	                otherIdentifyingInfo = otherIdentifyingInfo1; 
	              }
	              if (otherIdentifyingInfoSize == 2) {
	                otherIdentifyingInfo1 = otherIdentifyingInfoArray[0].toString();

	                String otherIdentifyingInfo2 = otherIdentifyingInfoArray[1].toString();

	                otherIdentifyingInfo = otherIdentifyingInfo1 + "," + otherIdentifyingInfo2;
	               // this.logger.debug("otherIdentifyingInfo1 = " + otherIdentifyingInfo1 + " otherIdentifyingInfo2 = " + otherIdentifyingInfo2);
	              }
	            } catch (Exception e) {
	              //this.logger.error("ERROR", e);
	            }

	            int statusInfo = 555;
	            boolean statusInfoDiscovery = true;
	            try {
	              statusInfo = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "StatusInfo").intValue();
	            } catch (NullPointerException npe) {
	              statusInfoDiscovery = false;
	            }

	            int requestedState = 0;
	            try {
	              requestedState = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "RequestedState").intValue();
	            } catch (RuntimeException e) {
	            //  this.logger.debug("No RequestedState for Volume");
	            }

	            int enabledState = 0;
	            try {
	              enabledState = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "EnabledState").intValue();
	            } catch (RuntimeException e) {
	             // this.logger.debug("No EnabledState for Volume");
	            }

	            int healthState = 0;
	            try {
	              healthState = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "HealthState").intValue();
	            } catch (RuntimeException e) {
	              //this.logger.debug("No HealthState for Volume");
	            }

	            try
	            {
	              UnsignedInteger16[] operationalStatusArray = this.cim_DT.getUint16ArrayPropertyValue(cim_StorageVolumeCI, "OperationalStatus");
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
	              csv.setOperationalStatus(operationalStatusFinal);
	            } catch (Exception e) {
	              csv.setOperationalStatus("Unknown");
	            //  this.logger.error("Operational Status", e);
	            }

	            String elementNameVolume;
	            try
	            {
	              elementNameVolume = this.cim_DT.getCIMInstancePropertyValueString(cim_StorageVolumeCI, "ElementName");
	            } catch (Exception e1) {
	              elementNameVolume = "Unknown";
	            }

	            csv.setDeviceId(deviceId);
	            csv.setEnabledDefault(enabledDefault);
	            if (seqentialAccessDiscovery) {
	              csv.setSequentialAccess(seqentialAccess);
	            }
	            if (nameNameSpaceDiscovery) {
	              csv.setNameNamespace(nameNameSpace);
	            }
	            csv.setNameFormat(nameFormat);
	            csv.setName(name);
	            csv.setPrimordial(primordialVolume);
	            csv.setDeltaReservation(deltaReservation);
	            csv.setPackageRedundancy(packageRedundancy);
	            csv.setDataRedundancy(dataRedundancy);
	            csv.setNoSinglePointOfFailure(noSinglePointOfFailure);
	            csv.setIsBasedOnUR(isBasedOnUnderlyingRed);
	            csv.setConsumableBlocks(consumableBlocks);
	            csv.setNumberOfBlocks(numberOfBlocks);
	            csv.setBlockSize(blockSize);
	            csv.setPoolName(poolName);
	            if (accessDiscovery) {
	              csv.setAccess(access);
	            }
	            csv.setPurpose(purpose);
	            csv.setIdentifyingDescriptions(identifyingDescriptions);
	            csv.setOtherIdentifyingInfo(otherIdentifyingInfo);
	            if (statusInfoDiscovery) {
	              csv.setStatusInfo(statusInfo);
	            }
	            csv.setRequestedState(requestedState);
	            csv.setEnabledState(enabledState);
	            csv.setHealthState(healthState);

	            csv.setElementName(elementNameVolume);

	            //--------------
	            csp.getCim_StorageVolume().add(csv);
	            
	            this.v7000StorageVolumes.add(csv);
	          }
	        }
	      }
	    }
	    catch (WBEMException ce) {
	      //this.logger.error(this.CN, ce);
	      return false;
	    }
	    return true;
	    
  }

  public boolean mapVolumeData()
  {
    try {
      long instanceTimeMeanPool = 0L;
      try {
        instanceTimeMeanPool = this.pm.AssociatorsTime(this.cc, this.instanceCOP, 
          "CIM_HostedStoragePool", "CIM_StoragePool", 
          "GroupComponent", "PartComponent");
      }
      catch (Exception localException1)
      {
      }

      CloseableIterator cim_StoragePoolEnum = this.cc.associators(this.instanceCOP, 
        "CIM_HostedStoragePool", "CIM_StoragePool", 
        "GroupComponent", "PartComponent", false, false, 
        null);

      while (cim_StoragePoolEnum.hasNext())
      {
     //   this.logger.debug(this.CN + " Enumerated StoragePool and has more elements");
        CIMInstance cim_StoragePoolCI = (CIMInstance)cim_StoragePoolEnum.next();
        CIMObjectPath cim_StoragePoolInstanceCOP = cim_StoragePoolCI.getObjectPath();
        CIM_StoragePool csp = new CIM_StoragePool();

        int instancePropertySize = cim_StoragePoolCI.getPropertyCount();
        csp.setInstancePropertySize(instancePropertySize);
     //   this.logger.info("InstancePropertySize = " + instancePropertySize);
        csp.setInstanceTimeMean(Long.valueOf(instanceTimeMeanPool));

        if (cim_StoragePoolEnum != null) {
          String instanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_StoragePoolCI, "InstanceID");
          String elementName;
          try {
            elementName = this.cim_DT.getCIMInstancePropertyValueString(cim_StoragePoolCI, "ElementName");
          } catch (Exception e4) {
            elementName = "Unknown";
          }
          boolean primordial;
          try {
            primordial = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_StoragePoolCI, "Primordial").booleanValue();
          } catch (Exception e) {
            primordial = false;
          }

          try
          {
            UnsignedInteger16[] operationalStatusArray = this.cim_DT.getUint16ArrayPropertyValue(cim_StoragePoolCI, "OperationalStatus");
            int operationalStatusSize = 0;
            if (operationalStatusArray != null) {
              operationalStatusSize = operationalStatusArray.length;
            }
         //   this.logger.debug("opearationalStatusSize = " + operationalStatusSize);
            Vector operationalStatusString = new Vector();
            for (int x = 0; x < operationalStatusSize; ++x)
            {
              UnsignedInteger16 opstsint = operationalStatusArray[x];

              int operationalStatusInt = Integer.parseInt(opstsint.toString());
              String operationalStatusValue = this.cim_Q.diskOperationalStatus(operationalStatusInt);

              operationalStatusString.add(operationalStatusValue);
            }

            String operationalStatusFinal = this.cim_Q.buildStringFromVector(operationalStatusString, ",");
            csp.setOperationalStatus(operationalStatusFinal);
          } catch (Exception operationalStatusArray) {
            csp.setOperationalStatus("Unknown");
          //  this.logger.error("Operational Status", e);
          }

          String poolId = "Not Available";
          try {
            poolId = this.cim_DT.getCIMInstancePropertyValueString(cim_StoragePoolCI, "PoolID");
          } catch (Exception e) {
          //  this.logger.error(this.CN, e);
          }
          Long totalManagedSpace = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StoragePoolCI, "TotalManagedSpace").longValue());
          Long remainingManagedSpace = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StoragePoolCI, "RemainingManagedSpace").longValue());

          csp.setInstanceId(instanceId);
          csp.setElementName(elementName);
          csp.setPrimordial(primordial);
          csp.setPoolId(poolId);
          csp.setTotalManagedSpace(totalManagedSpace);
          csp.setRemainingManagedSpace(remainingManagedSpace);

          CloseableIterator cim_AllocatedFromStoragePoolEnum = this.cc.associators(cim_StoragePoolInstanceCOP, 
            "CIM_AllocatedFromStoragePool", "CIM_StoragePool", 
            "Dependent", "Antecedent", false, false, 
            null);

          while (cim_AllocatedFromStoragePoolEnum.hasNext()) {
           // this.logger.debug(this.CN + " Enumerated AllocatedFromStoragePool and has more elements");
            CIMInstance cim_AllocatedFromStoragePoolCI = (CIMInstance)cim_AllocatedFromStoragePoolEnum.next();

            if (cim_AllocatedFromStoragePoolEnum != null) {
              String allocatedFromStoragePoolInstanceId = null;
              try {
                allocatedFromStoragePoolInstanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_AllocatedFromStoragePoolCI, "InstanceID");
                csp.setAllocatedFromStoragePool(allocatedFromStoragePoolInstanceId);
              }
              catch (Exception localException2)
              {
              }
            }

          }

          //this.sessionMapVolumes.save(csp);
          //this.csps.add(csp);
          /*
          String storagePoolID = this.sessionMapVolumes.getIdentifier(csp).toString();
          Integer storagePoolIDp = Integer.valueOf(storagePoolID);
         // this.logger.info("storagePoolIDp = " + storagePoolID);
          CIM_ComputerSystem aComputerSystem = (CIM_ComputerSystem)this.sessionMapVolumes.load(CIM_ComputerSystem.class, this.computerSystemID);
          CIM_StoragePool aStoragePool = (CIM_StoragePool)this.sessionMapVolumes.load(CIM_StoragePool.class, storagePoolIDp);
          aComputerSystem.getCim_StoragePool().add(aStoragePool);
          this.sessionMapVolumes.save(aComputerSystem);
          */
          //--------------------fill StoragePool------------
          this.ComputerSystem.getCim_StoragePool().add(csp);

          long statInstanceMeanExtent = 0L;
          try {
            statInstanceMeanExtent = this.pm.AssociatorsTime(this.cc, cim_StoragePoolInstanceCOP, "CIM_ConcreteComponent", 
              "CIM_StorageExtent", "GroupComponent", 
              "PartComponent");
          }
          catch (Exception localException3)
          {
          }

          CloseableIterator cim_StorageExtentEnum = this.cc
            .associators(cim_StoragePoolInstanceCOP, 
            "CIM_ConcreteComponent", 
            "CIM_StorageExtent", "GroupComponent", 
            "PartComponent", false, false, null);
          while (cim_StorageExtentEnum.hasNext())
          {
          //  this.logger.debug(this.CN + " Enumerated StorageExtent and has more elements");
            CIMInstance cim_StorageExtentCI = (CIMInstance)cim_StorageExtentEnum.next();

            CIM_StorageExtent cse = new CIM_StorageExtent();
            if (cim_StorageExtentEnum == null)
              continue;
            cse.setInstanceTimeMean(Long.valueOf(statInstanceMeanExtent));
            int instancePropertySizeExtent = cim_StorageExtentCI.getPropertyCount();
            cse.setInstancePropertySize(instancePropertySizeExtent);
           // this.logger.debug("InstancePropertySizeExtent = " + instancePropertySizeExtent);

            Long storageExtentNumberOfBlocks = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StorageExtentCI, "NumberOfBlocks").longValue());
            Long storageExtentBlockSize = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StorageExtentCI, "BlockSize").longValue());

            cse.setNumberOfBlocks(storageExtentNumberOfBlocks);
            cse.setBlockSize(storageExtentBlockSize);
            
            csp.getCim_StorageExtent().add(cse);

           /*
            this.sessionMapVolumes.save(cse);
            String storageExtentID = this.sessionMapVolumes.getIdentifier(cse).toString();
            Integer storageExtentIDp = Integer.valueOf(storageExtentID);
           // this.logger.debug("storageExtentIDp = " + storageExtentID);
            CIM_StoragePool aStoragePoolForStorageExtent = (CIM_StoragePool)this.sessionMapVolumes.load(CIM_StoragePool.class, storagePoolIDp);
            CIM_StorageExtent aStorageExtent = (CIM_StorageExtent)this.sessionMapVolumes.load(CIM_StorageExtent.class, storageExtentIDp);
            aStoragePoolForStorageExtent.getCim_StorageExtent().add(aStorageExtent);
            this.sessionMapVolumes.save(aStoragePoolForStorageExtent);
            */
          }

          CloseableIterator cim_StorageCapabilitiesEnum = this.cc
            .associators(cim_StoragePoolInstanceCOP, 
            "CIM_ElementCapabilities", 
            "CIM_StorageCapabilities", "ManagedElement", 
            "Capabilities", false, false, null);
          while (cim_StorageCapabilitiesEnum.hasNext())
          {
          //  this.logger.info(this.CN + " Enumerated StorageCapabilities for Volume and has more elements");
            CIMInstance cim_StorageCapabilitiesCI = (CIMInstance)cim_StorageCapabilitiesEnum.next();

            CIM_StorageCapabilities csc = new CIM_StorageCapabilities();
            if (cim_StorageCapabilitiesEnum != null) {
              String storageCapabilitiesElementName = this.cim_DT.getCIMInstancePropertyValueString(cim_StorageCapabilitiesCI, "ElementName");
              csc.setElementName(storageCapabilitiesElementName);
              
              csp.getCim_StorageCapabilities().add(csc);

         /*     this.sessionMapVolumes.save(csc);
              String storageCapabilitiesID = this.sessionMapVolumes.getIdentifier(csc).toString();
              Integer storageCapabilitiesIDp = Integer.valueOf(storageCapabilitiesID);
          //    this.logger.debug("storageCapabilitiesIDp = " + storageCapabilitiesID);
              CIM_StoragePool aStoragePoolForStorageCapabilities = (CIM_StoragePool)this.sessionMapVolumes.load(CIM_StoragePool.class, storagePoolIDp);
              CIM_StorageCapabilities aStorageCapabilities = (CIM_StorageCapabilities)this.sessionMapVolumes.load(CIM_StorageCapabilities.class, storageCapabilitiesIDp);
              aStoragePoolForStorageCapabilities.getCim_StorageCapabilities().add(aStorageCapabilities);
              this.sessionMapVolumes.save(aStoragePoolForStorageCapabilities);
              */
              
            }

          }

          long statInstanceMeanVolume = 0L;
          try {
            statInstanceMeanVolume = this.pm.AssociatorsTime(this.cc, cim_StoragePoolInstanceCOP, 
              "CIM_AllocatedFromStoragePool", 
              "CIM_StorageVolume", "Antecedent", 
              "Dependent");
          }
          catch (Exception localException4)
          {
          }

          CloseableIterator cim_StorageVolumeEnum = this.cc
            .associators(cim_StoragePoolInstanceCOP, 
            "CIM_AllocatedFromStoragePool", 
            "CIM_StorageVolume", "Antecedent", 
            "Dependent", false, false, null);
          while (cim_StorageVolumeEnum.hasNext())
          {
          //  this.logger.info(this.CN + " Enumerated storagevolume and has more elements");
            CIMInstance cim_StorageVolumeCI = (CIMInstance)cim_StorageVolumeEnum.next();
            CIMObjectPath cim_StorageVolumeCOP = cim_StorageVolumeCI.getObjectPath();
            CIM_StorageVolume csv = new CIM_StorageVolume();
            if (cim_StorageVolumeEnum == null) {
              continue;
            }
            csv.setInstanceTimeMean(Long.valueOf(statInstanceMeanVolume));
            int instancePropertySizeVolume = cim_StorageVolumeCI.getPropertyCount();
            csv.setInstancePropertySize(instancePropertySizeVolume);
          //  this.logger.debug("InstancePropertySizeVolume = " + instancePropertySizeVolume);
            String deviceId;
            try
            {
              deviceId = this.cim_DT.getCIMInstancePropertyValueString(cim_StorageVolumeCI, "DeviceID");
            } catch (RuntimeException e3) {
              deviceId = null;
            }

            int enabledDefault = 0;
            try {
              enabledDefault = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "EnabledDefault").intValue();
            } catch (RuntimeException e) {
          //    this.logger.debug("No EnabledDefault for volume");
            }

            boolean seqentialAccess = false;

            boolean seqentialAccessDiscovery = true;
            try {
              seqentialAccess = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_StorageVolumeCI, "SeqentialAccess").booleanValue();
            } catch (Exception npe) {
              seqentialAccessDiscovery = false;
            }

            int nameNameSpace = 0;
            boolean nameNameSpaceDiscovery = true;
            try {
              nameNameSpace = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "NameNamespace").intValue();
            } catch (Exception npe) {
              nameNameSpaceDiscovery = false;
            }

            int nameFormat = 0;
            try {
              nameFormat = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "NameFormat").intValue();
            } catch (Exception e) {
           //   this.logger.debug("No NameFormat for Volume");
            }
            String name;
            try
            {
              name = this.cim_DT.getCIMInstancePropertyValueString(cim_StorageVolumeCI, "Name");
            } catch (Exception e2) {
              name = null;
            }
            boolean primordialVolume;
            try {
              primordialVolume = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_StorageVolumeCI, "Primordial").booleanValue();
            } catch (Exception e1) {
              primordialVolume = false;
            }

            int deltaReservation = 0;
            try {
              deltaReservation = this.cim_DT.getCIMInstancePropertyUnsignedInt8Value(cim_StorageVolumeCI, "DeltaReservation").intValue();
            } catch (Exception e) {
             // this.logger.debug("No DeltaReservation for Volume");
            }

            int packageRedundancy = 0;
            try {
              packageRedundancy = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "PackageRedundancy").intValue();
            } catch (Exception e) {
            //  this.logger.debug("No Package Redundancy for Volume");
            }

            int dataRedundancy = 0;
            try {
              dataRedundancy = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "DataRedundancy").intValue();
            } catch (Exception e) {
            //  this.logger.debug("No DataRedundancy for Volume");
            }
            boolean noSinglePointOfFailure;
            try {
              noSinglePointOfFailure = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_StorageVolumeCI, "NoSinglePointOfFailure").booleanValue();
            } catch (Exception e) {
              noSinglePointOfFailure = false;
            }

            try
            {
              UnsignedInteger16[] extentStatus = this.cim_DT.getUint16ArrayPropertyValue(cim_StoragePoolCI, "ExtentStatus");
              int extentStatusSize = 0;
              if (extentStatus != null) {
                extentStatusSize = extentStatus.length;
              }
             // this.logger.debug("ExtentStatusDiskSize = " + extentStatusSize);
              Vector extentStatusString = new Vector();
              for (int x = 0; x < extentStatusSize; ++x)
              {
                int extentStatusInt = Integer.parseInt(extentStatus[x].toString());

                String extentStatusValue = this.cim_Q.extentStatus(extentStatusInt);

                extentStatusString.add(extentStatusValue);
              }

              String extentStatusFinal = this.cim_Q.buildStringFromVector(extentStatusString, ",");
              csv.setExtentStatus(extentStatusFinal);
            } catch (Exception extentStatus) {
             // this.logger.error("ExtentStatus", e);
              csv.setExtentStatus("Unknown");
            }
            boolean isBasedOnUnderlyingRed;
            try
            {
              isBasedOnUnderlyingRed = this.cim_DT.getCIMInstancePropertyBooleanValue(cim_StorageVolumeCI, "IsBasedOnUnderlyingRedundancy").booleanValue();
            } catch (Exception e) {
              isBasedOnUnderlyingRed = false;
            }
            Long consumableBlocks;
            try {
              consumableBlocks = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StorageVolumeCI, "ConsumableBlocks").longValue());
            } catch (NullPointerException npe) {
              consumableBlocks = null;
            }
            Long numberOfBlocks = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StorageVolumeCI, "NumberOfBlocks").longValue());
            Long blockSize = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_StorageVolumeCI, "BlockSize").longValue());

            int access = 555;
            boolean accessDiscovery = true;
            try {
              access = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "Access").intValue();
            } catch (NullPointerException npe) {
              accessDiscovery = false;
            }
            String purpose = "Not Available";
            try {
              purpose = this.cim_DT.getCIMInstancePropertyValueString(cim_StorageVolumeCI, "Purpose");
            } catch (Exception e) {
            //  this.logger.error(this.CN, e);
            }

            String identifyingDescriptions = null;
            try
            {
              String[] identifyingDescriptionsArray = this.cim_DT.getStringArrayPropertyValue(cim_StorageVolumeCI, "IdentifyingDescriptions");
              int identifyingDescriptionsSize = 0;
              if (identifyingDescriptionsArray != null) {
                identifyingDescriptionsSize = identifyingDescriptionsArray.length;
              }

             // this.logger.debug("identifyingDescriptionsSize = " + identifyingDescriptionsSize);
              String identifyingDescriptions1=null;
              if (identifyingDescriptionsSize == 1) {
                identifyingDescriptions1 = identifyingDescriptionsArray[0].toString();
                identifyingDescriptions=identifyingDescriptions1;
              }
              if (identifyingDescriptionsSize == 2) {
                identifyingDescriptions1 = identifyingDescriptionsArray[0].toString();

                String identifyingDescriptions2 = identifyingDescriptionsArray[1].toString();

                identifyingDescriptions = identifyingDescriptions1 + "," + identifyingDescriptions2;
                //this.logger.debug("identifyingDescriptions1 = " + identifyingDescriptions1 + " identifyingDescriptions2 = " + identifyingDescriptions2);
              }
            } catch (Exception e) {
            //  this.logger.error("ERROR", e);
            }

            String otherIdentifyingInfo = null;
            try
            {
              String[] otherIdentifyingInfoArray = this.cim_DT.getStringArrayPropertyValue(cim_StorageVolumeCI, "OtherIdentifyingInfo");
              int otherIdentifyingInfoSize = 0;
              if (otherIdentifyingInfoArray != null) {
                otherIdentifyingInfoSize = otherIdentifyingInfoArray.length;
              }

             // this.logger.debug("otherIdentifyingInfoSize = " + otherIdentifyingInfoSize);
              String otherIdentifyingInfo1;
              if (otherIdentifyingInfoSize == 1) {
                otherIdentifyingInfo1 = otherIdentifyingInfoArray[0].toString();

                otherIdentifyingInfo = otherIdentifyingInfo1; 
              }
              if (otherIdentifyingInfoSize == 2) {
                otherIdentifyingInfo1 = otherIdentifyingInfoArray[0].toString();

                String otherIdentifyingInfo2 = otherIdentifyingInfoArray[1].toString();

                otherIdentifyingInfo = otherIdentifyingInfo1 + "," + otherIdentifyingInfo2;
               // this.logger.debug("otherIdentifyingInfo1 = " + otherIdentifyingInfo1 + " otherIdentifyingInfo2 = " + otherIdentifyingInfo2);
              }
            } catch (Exception e) {
              //this.logger.error("ERROR", e);
            }

            int statusInfo = 555;
            boolean statusInfoDiscovery = true;
            try {
              statusInfo = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "StatusInfo").intValue();
            } catch (NullPointerException npe) {
              statusInfoDiscovery = false;
            }

            int requestedState = 0;
            try {
              requestedState = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "RequestedState").intValue();
            } catch (RuntimeException e) {
            //  this.logger.debug("No RequestedState for Volume");
            }

            int enabledState = 0;
            try {
              enabledState = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "EnabledState").intValue();
            } catch (RuntimeException e) {
             // this.logger.debug("No EnabledState for Volume");
            }

            int healthState = 0;
            try {
              healthState = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_StorageVolumeCI, "HealthState").intValue();
            } catch (RuntimeException e) {
              //this.logger.debug("No HealthState for Volume");
            }

            try
            {
              UnsignedInteger16[] operationalStatusArray = this.cim_DT.getUint16ArrayPropertyValue(cim_StorageVolumeCI, "OperationalStatus");
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
              csv.setOperationalStatus(operationalStatusFinal);
            } catch (Exception e) {
              csv.setOperationalStatus("Unknown");
            //  this.logger.error("Operational Status", e);
            }

            String elementNameVolume;
            try
            {
              elementNameVolume = this.cim_DT.getCIMInstancePropertyValueString(cim_StorageVolumeCI, "ElementName");
            } catch (Exception e1) {
              elementNameVolume = "Unknown";
            }

            csv.setDeviceId(deviceId);
            csv.setEnabledDefault(enabledDefault);
            if (seqentialAccessDiscovery) {
              csv.setSequentialAccess(seqentialAccess);
            }
            if (nameNameSpaceDiscovery) {
              csv.setNameNamespace(nameNameSpace);
            }
            csv.setNameFormat(nameFormat);
            csv.setName(name);
            csv.setPrimordial(primordialVolume);
            csv.setDeltaReservation(deltaReservation);
            csv.setPackageRedundancy(packageRedundancy);
            csv.setDataRedundancy(dataRedundancy);
            csv.setNoSinglePointOfFailure(noSinglePointOfFailure);
            csv.setIsBasedOnUR(isBasedOnUnderlyingRed);
            csv.setConsumableBlocks(consumableBlocks);
            csv.setNumberOfBlocks(numberOfBlocks);
            csv.setBlockSize(blockSize);
            if (accessDiscovery) {
              csv.setAccess(access);
            }
            csv.setPurpose(purpose);
            csv.setIdentifyingDescriptions(identifyingDescriptions);
            csv.setOtherIdentifyingInfo(otherIdentifyingInfo);
            if (statusInfoDiscovery) {
              csv.setStatusInfo(statusInfo);
            }
            csv.setRequestedState(requestedState);
            csv.setEnabledState(enabledState);
            csv.setHealthState(healthState);

            csv.setElementName(elementNameVolume);

            //--------------
            csp.getCim_StorageVolume().add(csv);
         /*   this.sessionMapVolumes.save(csv);
            String storageVolumeID = this.sessionMapVolumes.getIdentifier(csv).toString();
            Integer storageVolumeIDp = Integer.valueOf(storageVolumeID);
           // this.logger.info("storageVolumeIDp = " + storageVolumeID);
            CIM_StoragePool aStoragePoolForVolume = (CIM_StoragePool)this.sessionMapVolumes.load(CIM_StoragePool.class, storagePoolIDp);
            CIM_StorageVolume aStorageVolume = (CIM_StorageVolume)this.sessionMapVolumes.load(CIM_StorageVolume.class, storageVolumeIDp);
            aStoragePoolForVolume.getCim_StorageVolume().add(aStorageVolume);
            this.sessionMapVolumes.save(aStoragePoolForVolume);
              */
            if (cim_StorageVolumeEnum != null) {
            //  this.logger.info("storagevolume enum not null");
              CloseableIterator cim_BlockStorageStatisticalDataEnum1 = null;
              try {
                cim_BlockStorageStatisticalDataEnum1 = this.cc.associators(cim_StorageVolumeCOP, "CIM_ElementStatisticalData", "CIM_BlockStorageStatisticalData", "ManagedElement", "Stats", false, false, null);

                while (cim_BlockStorageStatisticalDataEnum1.hasNext()) {
               //   this.logger.info("enumerated BlockStorageStatisticalData for volume and has more elements");
                  CIMInstance cim_BlockStorageStatisticalDataCI = (CIMInstance)cim_BlockStorageStatisticalDataEnum1.next();

                  CIM_BlockStorageStatisticalData cbssd = new CIM_BlockStorageStatisticalData();
                  if (cim_BlockStorageStatisticalDataEnum1 != null) {
                    int elementType = 0;
                    try {
                      elementType = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_BlockStorageStatisticalDataCI, "ElementType").intValue();
                      cbssd.setElementType(elementType);
                    } catch (Exception e1) {
                      elementType = 0;
                    }
                    Long totalIOs;
                    try
                    {
                      totalIOs = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "TotalIOs").longValue());
                    } catch (NullPointerException npe) {
                      totalIOs = null;
                    }
                    cbssd.setTotalIos(totalIOs);
                    Long kBytesTransferred;
                    try
                    {
                      kBytesTransferred = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "KBytesTransferred").longValue());
                    } catch (NullPointerException npe) {
                      kBytesTransferred = null;
                    }
                    cbssd.setKBytesTransferred(kBytesTransferred);
                    Long readIOs;
                    try
                    {
                      readIOs = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "ReadIOs").longValue());
                    } catch (NullPointerException npe) {
                      readIOs = null;
                    }
                    cbssd.setReadHitIos(readIOs);
                    Long readHitIOs;
                    try
                    {
                      readHitIOs = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "ReadHitIOs").longValue());
                    } catch (NullPointerException npe) {
                      readHitIOs = null;
                    }
                    cbssd.setReadHitIos(readHitIOs);
                    Long writeIOs;
                    try
                    {
                      writeIOs = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "WriteIOs").longValue());
                    } catch (NullPointerException npe) {
                      writeIOs = null;
                    }
                    cbssd.setWriteIos(writeIOs);
                    Long writeHitIOs;
                    try
                    {
                      writeHitIOs = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_BlockStorageStatisticalDataCI, "WriteHitIOs").longValue());
                    } catch (NullPointerException npe) {
                      writeHitIOs = null;
                    }
                    cbssd.setWriteHitIos(writeHitIOs);
                    try
                    {
                      String instanceIdStat = this.cim_DT.getCIMInstancePropertyValueString(cim_BlockStorageStatisticalDataCI, "InstanceID");
                      cbssd.setInstanceId(instanceIdStat);
                    }
                    catch (NullPointerException localNullPointerException1) {
                    }
                    try {
                      CIMDateTime statisticTime = this.cim_DT.getCIMInstancePropertyDateTime(cim_BlockStorageStatisticalDataCI, "StatisticTime");
                      Calendar cal = this.cim_DT.getCalendarFromCIMDateTime(statisticTime);
                      cbssd.setStatisticTime(cal);
                    } catch (Exception e) {
                    //  this.logger.error("CS BSP Error", e);
                    }
                    csv.getCim_BlockStorageStatisticalData().add(cbssd);
                   /* this.sessionMapVolumes.save(cbssd);
                    String blockStorageStatisticalDataID = this.sessionMapVolumes.getIdentifier(cbssd).toString();
                    Integer blockStorageStatisticalDataIDp = Integer.valueOf(blockStorageStatisticalDataID);
                   // this.logger.info("blockStorageStatisticalDataIDp = " + blockStorageStatisticalDataIDp);
                   // this.logger.info("blockStorageStatisticalDataID = " + blockStorageStatisticalDataID);
                    CIM_StorageVolume aStorageVolumeForStats = (CIM_StorageVolume)this.sessionMapVolumes.load(CIM_StorageVolume.class, storageVolumeIDp);
                    CIM_BlockStorageStatisticalData aBlockStorageStatisticalData = (CIM_BlockStorageStatisticalData)this.sessionMapVolumes.load(CIM_BlockStorageStatisticalData.class, blockStorageStatisticalDataIDp);
                    aStorageVolumeForStats.getCim_BlockStorageStatisticalData().add(aBlockStorageStatisticalData);
                    this.sessionMapVolumes.save(aStorageVolumeForStats);
                    */

                   // CCalculationsBSPVolume calculateVolumeDelta = new CCalculationsBSPVolume();
                   // calculateVolumeDelta.calculateVolumeMetric(this.computerSystemID.intValue(), storagePoolIDp.intValue(), storageVolumeIDp.intValue(), csv, cbssd);
                  }
                }
              } catch (Exception e) {
                //this.logger.error("no stats for volume");
              }
            }
          }
        }
      }
    }
    catch (WBEMException ce) {
      //this.logger.error(this.CN, ce);
      return false;
    }
    return true;
  }
}