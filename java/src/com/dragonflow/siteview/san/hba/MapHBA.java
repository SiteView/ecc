package com.dragonflow.siteview.san.hba;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Vector;
import javax.cim.CIMDateTime;
import javax.cim.CIMDateTimeAbsolute;
import javax.cim.CIMInstance;
import javax.cim.CIMObjectPath;
import javax.cim.UnsignedInteger16;
import javax.cim.UnsignedInteger64;
import javax.wbem.CloseableIterator;
import javax.wbem.WBEMException;
import javax.wbem.client.WBEMClient;
import com.dragonflow.siteview.san.util.*;
import com.dragonflow.siteview.san.beans.*;
//import org.hibernate.Session;


public class MapHBA
{
  private String CN = "MapHBA";
  private CIM_DataTypes cim_DT = new CIM_DataTypes();
  private CIM_Qualifiers cim_Q = new CIM_Qualifiers();
  private WBEMClient cc;
  private CIMObjectPath instanceCOP;
 // private Integer computerSystemID;
  public CIM_ComputerSystem ComputerSystem;
  //private Session sessionMapHBA;
  ConversionUtil cu = new ConversionUtil();
  
  CIM_PortController tempPortController;
  CIM_FCPort tempFCPort;

  public MapHBA( WBEMClient cc1, CIMObjectPath instanceCOP1, CIM_ComputerSystem ComputerSystem) {
 //   this.sessionMapHBA = session1;
    this.cc = cc1;
    this.instanceCOP = instanceCOP1;
    this.ComputerSystem=ComputerSystem;
  //  this.computerSystemID = computerSystemID1;
//    this.logger.debug("Session1 = " + session1);
//    this.logger.debug("cc1 = " + cc1);
//    this.logger.debug("instaceCOP1 = " + instanceCOP1);
//    this.logger.debug("ComputerSystemID1 = " + computerSystemID1);
    mapHBAPhysicalPackageData();
  }

  public boolean mapHBAPhysicalPackageData()
  {
    try
    {
      CloseableIterator cim_PortControllerEnum = this.cc.associators(this.instanceCOP, 
        "CIM_SystemDevice", "CIM_PortController", 
        "GroupComponent", "PartComponent", false, false, 
        null);

      while (cim_PortControllerEnum.hasNext())
      {
//        this.logger.debug(this.CN + " Enumerated PortController and has more elements");
        CIMInstance cim_PortControllerCI = (CIMInstance)cim_PortControllerEnum.next();
        CIMObjectPath cim_PortControllerInstanceCOP = cim_PortControllerCI.getObjectPath();
        tempPortController= new CIM_PortController();
        if (cim_PortControllerEnum == null)
          continue;
        try
        {
          UnsignedInteger16[] operationalStatus = this.cim_DT.getUint16ArrayPropertyValue(cim_PortControllerCI, "OperationalStatus");
          int operationalStatusSize = 0;
          if (operationalStatus != null) {
            operationalStatusSize = operationalStatus.length;
          }
//          this.logger.debug("operationalStatusSize = " + operationalStatusSize);
          Vector operationalStatusString = new Vector();
          for (int x = 0; x < operationalStatusSize; ++x)
          {
            int operationalStatusInt = Integer.parseInt(operationalStatus[x].toString());

            String operationalStatusValue = this.cim_Q.operationalStatus(operationalStatusInt);

            operationalStatusString.add(operationalStatusValue);
          }

          String portOperationalStatusFinal = this.cim_Q.buildStringFromVector(operationalStatusString, ",");
          tempPortController.setOperationalStatus(portOperationalStatusFinal);
        } catch (Exception e) {
        	tempPortController.setOperationalStatus("Unknown");
        }

        String elementName = this.cim_DT.getCIMInstancePropertyValueString(cim_PortControllerCI, "ElementName");
        tempPortController.setElementName(elementName);
        String deviceId = this.cim_DT.getCIMInstancePropertyValueString(cim_PortControllerCI, "DeviceID");
        tempPortController.setDeviceId(deviceId);
        
        //------
        this.ComputerSystem.getCim_PortController().add(tempPortController);

        /*
        this.sessionMapHBA.save(cpc);
        String portControllerID = this.sessionMapHBA.getIdentifier(cpc).toString();
        Integer portControllerIDp = Integer.valueOf(portControllerID);
//        this.logger.debug("portControllerIDp = " + portControllerID);
        CIM_ComputerSystem aComputerSystem = (CIM_ComputerSystem)this.sessionMapHBA.load(CIM_ComputerSystem.class, this.computerSystemID);
        CIM_PortController aPortController = (CIM_PortController)this.sessionMapHBA.load(CIM_PortController.class, portControllerIDp);
        aComputerSystem.getCim_PortController().add(aPortController);
        this.sessionMapHBA.save(aComputerSystem);
        */

        if (cim_PortControllerEnum != null) {
          CloseableIterator cim_PhysicalPackageEnum = this.cc.associators(cim_PortControllerInstanceCOP, "CIM_Realizes", "CIM_PhysicalPackage", "Dependent", "Antecedent", false, false, null);
          while (cim_PhysicalPackageEnum.hasNext())
          {
//            this.logger.debug(this.CN + "enumerated physicalpackage and has more elements");
            CIMInstance cim_PhysicalPackageCI = (CIMInstance)cim_PhysicalPackageEnum.next();
            CIM_PhysicalPackage cpp = new CIM_PhysicalPackage();
            String manufacturer = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "Manufacturer");
            String model = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "Model");
            String version = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "Version");
            String serialNumber = this.cim_DT.getCIMInstancePropertyValueString(cim_PhysicalPackageCI, "SerialNumber");

            cpp.setManufacturer(manufacturer);
            cpp.setModel(model);
            cpp.setVersion(version);
            cpp.setSerialNumber(serialNumber);
            
            
            //------
            tempPortController.getCim_PhysicalPackage().add(cpp);

            /*
            this.sessionMapHBA.save(cpp);
            String physicalPackageID = this.sessionMapHBA.getIdentifier(cpp).toString();
            Integer physicalPackageIDp = Integer.valueOf(physicalPackageID);
//            this.logger.debug("physicalPackageIDp = " + physicalPackageID);
            CIM_PortController aPortControllerForPhysicalPackage = (CIM_PortController)this.sessionMapHBA.load(CIM_PortController.class, portControllerIDp);
            CIM_PhysicalPackage aPhysicalPackage = (CIM_PhysicalPackage)this.sessionMapHBA.load(CIM_PhysicalPackage.class, physicalPackageIDp);
            aPortControllerForPhysicalPackage.getCim_PhysicalPackage().add(aPhysicalPackage);
            this.sessionMapHBA.save(aPortControllerForPhysicalPackage);
            */
          }

          CloseableIterator cim_SoftwareIdentityEnum = this.cc.associators(cim_PortControllerInstanceCOP, "CIM_ElementSoftwareIdentity", "CIM_SoftwareIdentity", "Dependent", "Antecedent", false, false, null);
          while (cim_SoftwareIdentityEnum.hasNext())
          {
//            this.logger.debug(this.CN + "enumerated SoftwareIdentity and has more elements");
            CIMInstance cim_SoftwareIdentityCI = (CIMInstance)cim_SoftwareIdentityEnum.next();
            CIM_SoftwareIdentity csi = new CIM_SoftwareIdentity();
            if (cim_SoftwareIdentityEnum == null)
              continue;
            try {
              String instanceId = this.cim_DT.getCIMInstancePropertyValueString(cim_SoftwareIdentityCI, "InstanceID");
              csi.setInstanceId(instanceId);
            }
            catch (NullPointerException localNullPointerException1)
            {
            }
            try {
              String manufacturer = this.cim_DT.getCIMInstancePropertyValueString(cim_SoftwareIdentityCI, "Manufacturer");
              csi.setManufacturer(manufacturer);
            }
            catch (NullPointerException localNullPointerException2)
            {
            }
            try {
              String versionString = this.cim_DT.getCIMInstancePropertyValueString(cim_SoftwareIdentityCI, "VersionString");
              csi.setVersionString(versionString);
            }
            catch (NullPointerException localNullPointerException3)
            {
            }
            try
            {
              Vector classificationVector = this.cim_DT.getCIMInstancePropertyValueVector(cim_SoftwareIdentityCI, "Classifications");

              int classificationSize = 0;
              if (classificationVector != null) {
                classificationSize = classificationVector.size();
              }

              int intClassification1 = 0;
              String classification1;
              if (classificationSize == 1) {
                classification1 = classificationVector.get(0).toString();
                intClassification1 = Integer.parseInt(classification1);
              }
              else if (classificationSize == 2) {
                classification1 = classificationVector.get(0).toString();
                intClassification1 = Integer.parseInt(classification1);
              }
             // this.logger.debug("Classification = " + intClassification1);
              String classificationQualifier = this.cim_Q.softwareClassifications(intClassification1);
              csi.setClassification(classificationQualifier);
            }
            catch (NullPointerException localNullPointerException4)
            {
            }

            //--------
            tempPortController.getCim_SoftwareIdentity().add(csi);
            
            /*
            this.sessionMapHBA.save(csi);
            String softwareIdentityID = this.sessionMapHBA.getIdentifier(csi).toString();
            Integer softwareIdentityIDp = Integer.valueOf(softwareIdentityID);
//            this.logger.debug("softwareIdentityIDp = " + softwareIdentityID);
            CIM_PortController aPortControllerForSoftwareIdentity = (CIM_PortController)this.sessionMapHBA.load(CIM_PortController.class, portControllerIDp);
            CIM_SoftwareIdentity aSoftwareIdentity = (CIM_SoftwareIdentity)this.sessionMapHBA.load(CIM_SoftwareIdentity.class, softwareIdentityIDp);
            aPortControllerForSoftwareIdentity.getCim_SoftwareIdentity().add(aSoftwareIdentity);
            this.sessionMapHBA.save(aPortControllerForSoftwareIdentity);
            */
          }

          CloseableIterator cim_FCPortEnum = this.cc.associators(cim_PortControllerInstanceCOP, "CIM_ControlledBy", "CIM_FCPort", "Antecedent", "Dependent", false, false, null);
          while (cim_FCPortEnum.hasNext())
          {
//            this.logger.debug("enumerated FCPort and has more elements");
            CIMInstance cim_FCPortCI = (CIMInstance)cim_FCPortEnum.next();
            CIMObjectPath cim_FCPortCOP = cim_FCPortCI.getObjectPath();
            tempFCPort= new CIM_FCPort();
            if (cim_FCPortEnum != null) {
              String permanentAddress = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "PermanentAddress");
              String systemCreationClassNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "SystemCreationClassName");
              String systemNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "SystemName");
              String creationClassNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "CreationClassName");
              String deviceIdPort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "DeviceID");
              String elementNamePort = this.cim_DT.getCIMInstancePropertyValueString(cim_FCPortCI, "ElementName");
              int usageRestrictionPort;
              try {
                usageRestrictionPort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "UsageRestriction").intValue();
              } catch (Exception e1) {
                usageRestrictionPort = 0;
              }

              try
              {
                int portNumber = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "PortNumber").intValue();
                tempFCPort.setPortNumber(portNumber);
              }
              catch (Exception localException1) {
              }
              String usageRestrictionConversion = this.cim_Q.usageRestriction(usageRestrictionPort);
              try
              {
                UnsignedInteger16[] operationalStatusArray = this.cim_DT.getUint16ArrayPropertyValue(cim_FCPortCI, "OperationalStatus");
                int operationalStatusSize = 0;
                if (operationalStatusArray != null) {
                  operationalStatusSize = operationalStatusArray.length;
                }
//                this.logger.debug("opearationalStatusSize = " + operationalStatusSize);
                Vector operationalStatusString = new Vector();
                for (int x = 0; x < operationalStatusSize; ++x)
                {
                  UnsignedInteger16 opstsint = operationalStatusArray[x];

                  int operationalStatusInt = Integer.parseInt(opstsint.toString());
                  String operationalStatusValue = this.cim_Q.diskOperationalStatus(operationalStatusInt);

                  operationalStatusString.add(operationalStatusValue);
                }

                String operationalStatusFinal = this.cim_Q.buildStringFromVector(operationalStatusString, ",");
                tempFCPort.setOperationalStatus(operationalStatusFinal);
              } catch (Exception e) {
            	  tempFCPort.setOperationalStatus("Unknown");
//                this.logger.error("Operational Status", e);
              }

              try
              {
                Long speedPort = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "Speed").longValue());
                tempFCPort.setSpeed(speedPort);
              }
              catch (NullPointerException localNullPointerException5)
              {
              }
              try {
                Long maxSpeedPort = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "MaxSpeed").longValue());
                tempFCPort.setMaxSpeed(maxSpeedPort);
              } catch (Exception localException2) {
              }
              int portTypePort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "PortType").intValue();
              String portTypeConversion = this.cim_Q.portType(portTypePort);
              int linkTechnologyPort;
              try {
                linkTechnologyPort = this.cim_DT.getCIMInstancePropertyUnsignedInt16Value(cim_FCPortCI, "LinkTechnology").intValue();
                String linkTechnologyConversion = this.cim_Q.linkTechnology(linkTechnologyPort);
                tempFCPort.setLinkTechnology(linkTechnologyConversion);
              } catch (Exception e) {
                linkTechnologyPort = 555;
              }
              Long supportedMaximumTransmissionUnit = null;
              try {
                supportedMaximumTransmissionUnit = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortCI, "SupportedMaximumTransmissionUnit").longValue());
              } catch (Exception localException3) {
              }
//              this.logger.debug(this.CN + " FCPort WWN = " + permanentAddress);

              tempFCPort.setPermanentAddress(permanentAddress);
              tempFCPort.setSystemCreationClassName(systemCreationClassNamePort);
              tempFCPort.setSystemName(systemNamePort);
              tempFCPort.setCreationClassName(creationClassNamePort);
              tempFCPort.setDeviceId(deviceIdPort);
              tempFCPort.setElementName(elementNamePort);
              tempFCPort.setUsageRestriction(usageRestrictionConversion);
              tempFCPort.setPortType(portTypeConversion);
              tempFCPort.setSupportedMaximumTransmissionUnit(supportedMaximumTransmissionUnit);
              
              tempPortController.getCim_FCPort().add(tempFCPort);

              /*
              this.sessionMapHBA.save(cfcp);
              String fcPortID = this.sessionMapHBA.getIdentifier(cfcp).toString();
              Integer fcPortIDp = Integer.valueOf(fcPortID);
//              this.logger.debug("portIDp = " + fcPortID);
              CIM_PortController aPortControllerForFCPort = (CIM_PortController)this.sessionMapHBA.load(CIM_PortController.class, portControllerIDp);
              CIM_FCPort aFCPort = (CIM_FCPort)this.sessionMapHBA.load(CIM_FCPort.class, fcPortIDp);
              aPortControllerForFCPort.getCim_FCPort().add(aFCPort);
              this.sessionMapHBA.save(aPortControllerForFCPort);
              */
              CloseableIterator cim_LogicalPortGroupEnum = this.cc.associators(cim_FCPortCOP, "CIM_MemberOfCollection", "CIM_LogicalPortGroup", "Member", "Collection", false, false, null);

              while (cim_LogicalPortGroupEnum.hasNext()) {
               // this.logger.debug("enumerated LogicalPortGroup and has more elements");
                CIMInstance cim_LogicalPortGroupCI = (CIMInstance)cim_LogicalPortGroupEnum.next();

                CIM_LogicalPortGroup clpg = new CIM_LogicalPortGroup();
                if (cim_LogicalPortGroupCI == null) continue;
                String instanceIDGroup;
                try {
                  instanceIDGroup = this.cim_DT.getCIMInstancePropertyValueString(cim_LogicalPortGroupCI, "InstanceID").toString();
                } catch (NullPointerException npe) {
                  instanceIDGroup = null;
                }
                String captionGroup;
                try {
                  captionGroup = this.cim_DT.getCIMInstancePropertyValueString(cim_LogicalPortGroupCI, "Caption").toString();
                } catch (NullPointerException npe) {
                  captionGroup = null;
                }
                String descriptionGroup;
                try {
                  descriptionGroup = this.cim_DT.getCIMInstancePropertyValueString(cim_LogicalPortGroupCI, "Description").toString();
                } catch (NullPointerException npe) {
                  descriptionGroup = null;
                }
                String elementNameGroup;
                try {
                  elementNameGroup = this.cim_DT.getCIMInstancePropertyValueString(cim_LogicalPortGroupCI, "ElementName").toString();
                } catch (NullPointerException npe) {
                  elementNameGroup = null;
                }
                String nameGroup;
                try {
                  nameGroup = this.cim_DT.getCIMInstancePropertyValueString(cim_LogicalPortGroupCI, "Name").toString();
                } catch (NullPointerException npe) {
                  nameGroup = null;
                }
                String nameFormatGroup;
                try {
                  nameFormatGroup = this.cim_DT.getCIMInstancePropertyValueString(cim_LogicalPortGroupCI, "NameFormat").toString();
                } catch (NullPointerException npe) {
                  nameFormatGroup = null;
                }
                String otherNameFormatGroup;
                try {
                  otherNameFormatGroup = this.cim_DT.getCIMInstancePropertyValueString(cim_LogicalPortGroupCI, "OtherNameFormat").toString();
                } catch (NullPointerException npe) {
                  otherNameFormatGroup = null;
                }

                clpg.setInstanceId(instanceIDGroup);
                clpg.setCaption(captionGroup);
                clpg.setDescription(descriptionGroup);
                clpg.setElementName(elementNameGroup);
                clpg.setName(nameGroup);
                clpg.setNameFormat(nameFormatGroup);
                clpg.setOtherNameFormat(otherNameFormatGroup);
                
                
                tempFCPort.getCim_LogicalPortGroup().add(clpg);

                /*
                this.sessionMapHBA.save(clpg);
                String logicalPortGroupID = this.sessionMapHBA.getIdentifier(clpg).toString();
                Integer logicalPortGroupIDp = Integer.valueOf(logicalPortGroupID);
//                this.logger.debug("logicalPortGroupIDp = " + logicalPortGroupIDp);
                CIM_FCPort aFCPortForLogicalPortGroup = (CIM_FCPort)this.sessionMapHBA.load(CIM_FCPort.class, fcPortIDp);
                CIM_LogicalPortGroup aLogicalPortGroup = (CIM_LogicalPortGroup)this.sessionMapHBA.load(CIM_LogicalPortGroup.class, logicalPortGroupIDp);
                aFCPortForLogicalPortGroup.getCim_LogicalPortGroup().add(aLogicalPortGroup);
                this.sessionMapHBA.save(aFCPortForLogicalPortGroup);
                */
              }

              CloseableIterator cim_FCPortStatisticsEnum = this.cc.associators(cim_FCPortCOP, "CIM_ElementStatisticalData", "CIM_FCPortStatistics", "ManagedElement", "Stats", false, false, null);

              while (cim_FCPortStatisticsEnum.hasNext()) {
//                this.logger.debug("enumerated BlockStorageStatisticalData and has more elements");
                CIMInstance cim_FCPortStatisticsCI = (CIMInstance)cim_FCPortStatisticsEnum.next();

                CIM_FCPortStatistics cbssd = new CIM_FCPortStatistics();
                if (cim_FCPortStatisticsEnum == null) continue;
                Long bytesTransmitted;
                try {
                  bytesTransmitted = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "BytesTransmitted").longValue());
                } catch (NullPointerException elementNameGroup) {
                  bytesTransmitted = null;
                }
                Long bytesReceived;
                try {
                  bytesReceived = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "BytesReceived").longValue());
                } catch (NullPointerException nameGroup) {
                  bytesReceived = null;
                }
                Long packetsTransmitted;
                try {
                  packetsTransmitted = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "PacketsTransmitted").longValue());
                } catch (NullPointerException nameFormatGroup) {
                  packetsTransmitted = null;
                }
                Long packetsReceived;
                try {
                  packetsReceived = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "PacketsReceived").longValue());
                } catch (NullPointerException otherNameFormatGroup) {
                  packetsReceived = null;
                }
                Long crcErrors;
                try {
                  crcErrors = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "CRCErrors").longValue());
                } catch (NullPointerException logicalPortGroupID) {
                  crcErrors = null;
                }
                Long linkFailures;
                try {
                  linkFailures = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "LinkFailures").longValue());
                } catch (NullPointerException npe) {
                  linkFailures = null;
                }
                Long primitiveSeqProtocolErrCount;
                try {
                  primitiveSeqProtocolErrCount = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "PrimitiveSeqProtocolErrCount").longValue());
                } catch (NullPointerException npe) {
                  primitiveSeqProtocolErrCount = null;
                }
                Long lossOfSignalCounter;
                try {
                  lossOfSignalCounter = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "LossOfSignalCounter").longValue());
                } catch (NullPointerException npe) {
                  lossOfSignalCounter = null;
                }
                Long invalidTransmissionWords;
                try {
                  invalidTransmissionWords = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "InvalidTransmissionWords").longValue());
                } catch (NullPointerException npe) {
                  invalidTransmissionWords = null;
                }
                Long lipCount;
                try {
                  lipCount = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "LIPCount").longValue());
                } catch (NullPointerException npe) {
                  lipCount = null;
                }
                Long nosCount;
                try {
                  nosCount = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "NOSCount").longValue());
                } catch (NullPointerException npe) {
                  nosCount = null;
                }
                Long errorFrames;
                try {
                  errorFrames = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "ErrorFrames").longValue());
                } catch (NullPointerException npe) {
                  errorFrames = null;
                }
                Long dumpedFrames;
                try {
                  dumpedFrames = Long.valueOf(this.cim_DT.getCIMInstancePropertyUnsignedInt64Value(cim_FCPortStatisticsCI, "DumpedFrames").longValue());
                } catch (NullPointerException npe) {
                  dumpedFrames = null;
                }

                Calendar cal1 = Calendar.getInstance();
                try {
                  CIMDateTime statisticTime = this.cim_DT.getCIMInstancePropertyDateTime(cim_FCPortStatisticsCI, "StatisticTime");
                  String statisticTimeString = statisticTime.toString();
                  CIMDateTimeAbsolute cdt = new CIMDateTimeAbsolute(statisticTimeString);
                  cal1 = Calendar.getInstance();
                  cal1.set(1, cdt.getYear());
                  cal1.set(2, cdt.getMonth());
                  cal1.set(6, cdt.getDay());
                  cal1.set(10, cdt.getHour());
                  cal1.set(12, cdt.getMinute());
                  cal1.set(13, cdt.getSecond());
//                  this.logger.info("StatisticTime = " + statisticTime.toString());
                }
                catch (NullPointerException npe)
                {
//                  this.logger.info("StatisticTime on catch = " + cal1.toString());
                }

        /*  
         * 
         *       
         *       String sqlHBAPerformanceData = "SELECT cim_fcportstatistics.statisticTime, cim_fcportstatistics.bytesTransmitted, cim_fcportstatistics.bytesReceived, cim_fcportstatistics.packetsTransmitted,cim_fcportstatistics.packetsReceived FROM cim_computersystem AS ccs Inner Join cimomflag cf ON ccs.cimomID = cf.cimomID Inner Join cim_systemdevice_hba AS csd ON csd.computerSystemID = ccs.computerSystemID Inner Join cim_portcontroller AS cpc ON csd.portControllerID = cpc.portControllerID Inner Join cim_controlledby_hba AS ccb ON cpc.portControllerID = ccb.portControllerID Inner Join cim_fcport AS cfp ON ccb.portID = cfp.portID Inner Join cim_elementstatisticaldata_hba ON cfp.portID = cim_elementstatisticaldata_hba.portID Inner Join cim_fcportstatistics ON cim_elementstatisticaldata_hba.fcPortStatisticsID = cim_fcportstatistics.fcPortStatisticsID WHERE cim_fcportstatistics.statisticTime = (SELECT MAX(i2.statisticTime) FROM cim_fcport i1 Inner Join cim_elementstatisticaldata_hba ON i1.portID = cim_elementstatisticaldata_hba.portID Inner Join cim_fcportstatistics i2 ON cim_elementstatisticaldata_hba.fcPortStatisticsID = i2.fcPortStatisticsID WHERE i1.permanentAddress='" + 
                  permanentAddress + "') AND cf.deletion =  '0' AND cfp.permanentAddress = '" + permanentAddress + "'";
                Iterator i = this.sessionMapHBA.createSQLQuery(sqlHBAPerformanceData).list().iterator();
//                this.logger.info(sqlHBAPerformanceData);
                Long bytesTransmittedDiff = null;
                Long bytesReceivedDiff = null;
                Long packetsTransmittedDiff = null;
                Long packetsReceivedDiff = null;
//                this.logger.info("Port Performance Iterator hasNext = " + i.hasNext());
                if (i.hasNext()) {
                  while (i.hasNext())
                  {
                    Object[] row = (Object[])i.next();
                    String dbStatisticTime = row[0].toString();
//                    this.logger.info("DBStatisticTime = " + dbStatisticTime);

                    Date d1 = null;
                    Calendar cal = null;
                    try {
                      cal = Calendar.getInstance();
                      SimpleDateFormat df = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
                      d1 = df.parse(dbStatisticTime);
                      cal.setTime(d1);
//                      this.logger.info("DBDate = " + d1);
                    } catch (ParseException pe) {
//                      this.logger.info("MapHBA", pe);
                    }

//                    this.logger.info("DBStatisticTime = " + cal.getTimeInMillis());
//                    this.logger.info("StatisticTime = " + cal.getTimeInMillis());
//                    this.logger.info("DBStatisticTime = " + cal);
//                    this.logger.info("StatisticTime = " + cal1);
                    long diffMillis = cal.getTimeInMillis() - cal.getTimeInMillis();
//                    this.logger.info("StatisticDiff Milliseconds = " + diffMillis);

                    long diffMins = Math.abs(diffMillis / 60000L);
                    cbssd.setStatisticTimeDiff(Long.valueOf(diffMillis));
//                    this.logger.info("Date Diff Minutes = " + diffMins);

                    bytesTransmittedDiff = Long.valueOf((bytesTransmitted.longValue() - Long.valueOf(row[1].toString()).longValue()) / diffMins);
//                    this.logger.info("bytesTransmittedDiff = " + bytesTransmittedDiff);
                    bytesReceivedDiff = Long.valueOf((bytesReceived.longValue() - Long.valueOf(row[2].toString()).longValue()) / diffMins);
//                    this.logger.info("bytesReceivedDiff = " + bytesReceivedDiff);
                    packetsTransmittedDiff = Long.valueOf((packetsTransmitted.longValue() - Long.valueOf(row[3].toString()).longValue()) / diffMins);
//                    this.logger.info("packetsTransmittedDiff = " + packetsTransmittedDiff);
                    packetsReceivedDiff = Long.valueOf((packetsReceived.longValue() - Long.valueOf(row[4].toString()).longValue()) / diffMins);
//                    this.logger.info("packetsReceivedDiff = " + packetsReceivedDiff);
                  }

                }

                cbssd.setBytesTransmitted(bytesTransmitted);
                cbssd.setBytesTransmittedDiff(bytesTransmittedDiff);
                cbssd.setBytesReceived(bytesReceived);
                cbssd.setBytesReceivedDiff(bytesReceivedDiff);
                cbssd.setPacketsTransmitted(packetsTransmitted);
                cbssd.setPacketsTransmittedDiff(packetsTransmittedDiff);
                cbssd.setPacketsReceived(packetsReceived);
                cbssd.setPacketsReceivedDiff(packetsReceivedDiff);
                cbssd.setCrcErrors(crcErrors);
                cbssd.setLinkFailures(linkFailures);
                cbssd.setPrimitiveSeqProtocolErrCount(primitiveSeqProtocolErrCount);
                cbssd.setLossOfSignalCounter(lossOfSignalCounter);
                cbssd.setInvalidTransmissionWords(invalidTransmissionWords);
                cbssd.setLipCount(lipCount);

                cbssd.setNosCount(nosCount);

                cbssd.setErrorFrames(errorFrames);
                cbssd.setDumpedFrames(dumpedFrames);
                cbssd.setStatisticTime(cal1);
                this.sessionMapHBA.save(cbssd);

                String fcPortStatisticsID = this.sessionMapHBA.getIdentifier(cbssd).toString();
                Integer fcPortStatisticsIDp = Integer.valueOf(fcPortStatisticsID);
//                this.logger.debug("fcPortStatisticsIDp = " + fcPortStatisticsIDp);
//                this.logger.debug("fcPortStatisticsID = " + fcPortStatisticsID);

                CIM_FCPort aFCPortForStats = (CIM_FCPort)this.sessionMapHBA.load(CIM_FCPort.class, fcPortIDp);
                CIM_FCPortStatistics aFCPortStatistics = (CIM_FCPortStatistics)this.sessionMapHBA.load(CIM_FCPortStatistics.class, fcPortStatisticsIDp);
                aFCPort.getCim_FCPortStatistics().add(aFCPortStatistics);
                this.sessionMapHBA.save(aFCPortForStats);
               
                *
                */
              }
            }
          }
        }
      }
    }
    catch (WBEMException ce)
    {
//      this.logger.error("MapHBA", ce);
      return false;
    }
//    catch (HibernateException he) 
    catch (Exception he) 
    {
//      this.logger.error("MapHBA", he);
    }
    return true;
  }
}
